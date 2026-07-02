{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Real @cabal build@ runs and the parallel scheduler.
--
-- Each job is a plain piped subprocess.  Its stdout and stderr get a reader
-- thread each; both feed the single-writer 'Console' (the Leksah
-- @IDE.Utils.Tool@ discipline), so concurrent jobs can't garble each other's
-- compiler errors.  Every line is prefixed with the unit's name.
--
-- Scheduling: 'spawnBuild' is called for a unit as soon as its repl check
-- passes; the job waits for its local deps' builds, takes a @-j@ slot (and,
-- for library components, a per-package lock — two libs of one package must
-- not register concurrently), then builds.  The first failure flips
-- 'beFailed' and terminates the other in-flight jobs.
module FFCabal.Build
  ( BuildEnv(..)
  , newBuildEnv
  , runCabal
  , spawnBuild
  , waitUnit
  , waitAllBuilds
  , failNow
  , anyFailed
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception (SomeException, bracket, bracket_, catch, try)
import Control.Monad (forM_, unless, void)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose)
import System.Process

import FFCabal.Output (Console, cErr, cOut)
import FFCabal.Plan (PlanUnit(..), unitTarget)

data BuildEnv = BuildEnv
  { beConsole  :: Console
  , beJobs     :: QSem                          -- ^ @-j@ slots
  , beFailed   :: IORef Bool
  , beDone     :: M.Map Text (MVar Bool)        -- ^ unit id → build result
  , beThreads  :: MVar [MVar ()]                -- ^ join handles
  , bePkgLocks :: MVar (M.Map Text (MVar ()))   -- ^ per-package lib-registration locks
  , beProcs    :: MVar (M.Map Text ProcessHandle)
  }

newBuildEnv :: Console -> Int -> [Text] -> IO BuildEnv
newBuildEnv console jobs unitIds = do
    sem <- newQSem (max 1 jobs)
    failed <- newIORef False
    done <- M.fromList <$> mapM (\i -> (,) i <$> newEmptyMVar) unitIds
    threads <- newMVar []
    locks <- newMVar M.empty
    procs <- newMVar M.empty
    return $ BuildEnv console sem failed done threads locks procs

-- | Run @cabal ARGS@, streaming both pipes (prefixed) through the console.
-- The process gets its own group so it can be interrupted as a whole.
runCabal :: BuildEnv -> Text -> [String] -> IO Bool
runCabal env prefix args = do
    let console = beConsole env
        key = prefix <> T.pack (show args)
    r <- try $ createProcess (proc "cabal" args)
                 { std_out = CreatePipe, std_err = CreatePipe, create_group = True }
    case r of
      Left (e :: SomeException) -> do
          cErr console $ "[" <> prefix <> "] failed to start cabal: " <> T.pack (show e)
          return False
      Right (_, Just hout, Just herr, ph) -> do
          modifyMVar_ (beProcs env) (return . M.insert key ph)
          doneOut <- newEmptyMVar
          doneErr <- newEmptyMVar
          _ <- forkIO $ pump hout (cOut console) `finallyPut` doneOut
          _ <- forkIO $ pump herr (cErr console) `finallyPut` doneErr
          takeMVar doneOut
          takeMVar doneErr
          code <- waitForProcess ph
          modifyMVar_ (beProcs env) (return . M.delete key)
          return (code == ExitSuccess)
      Right _ -> return False
  where
    finallyPut act done = (act `catch` \(_ :: SomeException) -> return ()) >> putMVar done ()
    pump :: Handle -> (Text -> IO ()) -> IO ()
    pump h emit = loop `catch` \(_ :: SomeException) -> hClose h `catch` \(_ :: SomeException) -> return ()
      where
        loop = do
            l <- TIO.hGetLine h
            emit ("[" <> prefix <> "] " <> l)
            loop

-- | Queue a unit's real build (call after its repl check passed).
spawnBuild :: BuildEnv -> [String] -> PlanUnit -> [Text] -> IO ()
spawnBuild env cabalOpts u depIds = do
    join' <- newEmptyMVar
    modifyMVar_ (beThreads env) (return . (join' :))
    void . forkIO $ (`finallyPut` join') $ do
        depsOk <- and <$> mapM (waitUnit env) depIds
        bail <- readIORef (beFailed env)
        ok <- if not depsOk || bail
          then return False
          else bracket_ (waitQSem (beJobs env)) (signalQSem (beJobs env)) $
                 withRegistrationLock $
                   runCabal env (unitTarget u)
                     (["build", T.unpack (unitTarget u)] ++ cabalOpts)
        unless ok $ do
            already <- atomicModifyIORef' (beFailed env) (\f -> (True, f))
            unless (already || not depsOk || bail) $
                cErr (beConsole env) $ "ffcabal: build FAILED: " <> unitTarget u
            failNow env
        markDone ok
  where
    markDone ok = forM_ (M.lookup (puId u) (beDone env)) $ \mv -> void (tryPutMVar mv ok)
    finallyPut act done = (act `catch` \(e :: SomeException) -> do
                              cErr (beConsole env) $ "ffcabal: build thread died: " <> T.pack (show e)
                              atomicWriteIORef (beFailed env) True
                              markDone False)
                          >> putMVar done ()
    -- Two library components of the same package register into the same
    -- inplace package db; serialize those.  Executables run freely.
    withRegistrationLock act = case puComponent u of
        Just c | "lib" `T.isPrefixOf` c -> do
            lock <- modifyMVar (bePkgLocks env) $ \m ->
                case M.lookup (puPkgName u) m of
                    Just l  -> return (m, l)
                    Nothing -> do l <- newMVar (); return (M.insert (puPkgName u) l m, l)
            bracket (takeMVar lock) (putMVar lock) (const act)
        _ -> act

-- | Wait for a unit's build to finish; result (True = success).
waitUnit :: BuildEnv -> Text -> IO Bool
waitUnit env i = maybe (return True) readMVar (M.lookup i (beDone env))

-- | Join all spawned build threads; True if nothing failed.
waitAllBuilds :: BuildEnv -> IO Bool
waitAllBuilds env = do
    joins <- readMVar (beThreads env)
    mapM_ takeMVar joins
    not <$> readIORef (beFailed env)

-- | Flip the fail flag and terminate all in-flight cabal processes.
failNow :: BuildEnv -> IO ()
failNow env = do
    atomicWriteIORef (beFailed env) True
    procs <- readMVar (beProcs env)
    forM_ (M.elems procs) $ \ph -> do
        interruptProcessGroupOf ph `catch` \(_ :: SomeException) -> return ()
        terminateProcess ph `catch` \(_ :: SomeException) -> return ()

anyFailed :: BuildEnv -> IO Bool
anyFailed = readIORef . beFailed

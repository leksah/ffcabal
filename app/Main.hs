{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | @ffcabal@ — fail-fast cabal.  See README.md.
module Main (main) where

import Control.Monad (unless, when)
import Data.List (isPrefixOf, stripPrefix)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Conc (getNumProcessors)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitFailure, exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler(Catch), installHandler, sigINT, sigTERM)

import FFCabal.Build
import FFCabal.Graph
import FFCabal.Output
import FFCabal.Plan
import FFCabal.Repl

data Cmd = CmdBuild | CmdRepl | CmdCheck deriving Eq

data Opts = Opts
  { oCmd      :: Cmd
  , oTargets  :: [Text]
  , oBuilddir :: Maybe FilePath
  , oJobs     :: Maybe Int
  , oReplOnly :: Bool
  , oTimeout  :: Int
  , oCabal    :: [String]   -- pass-through cabal options
  }

usage :: String
usage = unlines
  [ "ffcabal — fail-fast cabal (cached tmux repl checks, then parallel builds)"
  , ""
  , "Usage:"
  , "  ffcabal build [TARGET…] [--builddir=DIR] [-jN] [--repl-only] [--timeout=SECS] [CABAL-OPTS…]"
  , "  ffcabal repl  TARGET   [--builddir=DIR] [CABAL-OPTS…]"
  , "  ffcabal check TARGET   [--builddir=DIR] [CABAL-OPTS…]"
  , ""
  , "build: dry-run to refresh plan.json; type-check each local component in"
  , "       dependency order in a cached 'cabal repl' (tmux session 'ffcabal',"
  , "       reused when the component's UnitId is unchanged); build each checked"
  , "       component in parallel; finish with one 'cabal build TARGET…'."
  , "repl:  ensure TARGET's repl exists, run a :reload check, show how to attach."
  , "check: like repl, but for scripts (exit status only, no attach hint)."
  , ""
  , "TARGETs: pkg | exe:name | lib:name | pkg:comp:name   (default: all local)"
  , "Unrecognised options are passed through to cabal.  Run from the project root."
  ]

main :: IO ()
main = getArgs >>= \case
    [] -> putStr usage >> exitFailure
    (c : _) | c `elem` ["-h", "--help", "help"] -> putStr usage
    args -> case parseArgs args of
        Left err   -> hPutStrLn stderr err >> exitFailure
        Right opts -> run opts

parseArgs :: [String] -> Either String Opts
parseArgs [] = Left usage
parseArgs (cmdW : rest) = do
    cmd <- case cmdW of
        "build" -> Right CmdBuild
        "repl"  -> Right CmdRepl
        "check" -> Right CmdCheck
        _       -> Left $ "ffcabal: unknown command '" <> cmdW <> "'\n\n" <> usage
    go (Opts cmd [] Nothing Nothing False 600 []) rest
  where
    go acc [] = Right acc
    go acc (a : as)
      | a == "--repl-only" = go acc { oReplOnly = True } as
      | Just v <- stripPrefix "--builddir=" a = go acc { oBuilddir = Just v } as
      | a == "--builddir", (v : as') <- as = go acc { oBuilddir = Just v } as'
      | Just v <- stripPrefix "--timeout=" a, Just n <- readInt v = go acc { oTimeout = n } as
      | Just v <- stripPrefix "--jobs=" a, Just n <- readInt v = go acc { oJobs = Just n } as
      | Just v <- stripPrefix "-j" a, Just n <- readInt v = go acc { oJobs = Just n } as
      | a == "-j", (v : as') <- as, Just n <- readInt v = go acc { oJobs = Just n } as'
      | "-" `isPrefixOf` a = go acc { oCabal = oCabal acc ++ [a] } as
      | otherwise = go acc { oTargets = oTargets acc ++ [T.pack a] } as
    readInt s = case reads s of [(n, "")] -> Just (n :: Int); _ -> Nothing

run :: Opts -> IO ()
run opts = withConsole $ \console -> do
    cwd <- getCurrentDirectory
    let planDir   = fromMaybe "dist-newstyle" (oBuilddir opts)
        cabalOpts = [ "--builddir=" <> d | Just d <- [oBuilddir opts] ] ++ oCabal opts
        -- Absolute: the pipe-pane capture command runs in the tmux server's
        -- context, so a relative log path would resolve who-knows-where.
        stateDir  = cwd </> planDir </> "ffcabal"
        envFile   = stateDir </> "env.sh"
        targetsS  = map T.unpack (oTargets opts)
        dryTargets = if null targetsS then ["all"] else targetsS
    createDirectoryIfMissing True stateDir
    writeEnvFile envFile

    -- 1. Plan.
    cOut console "ffcabal: planning (cabal build --dry-run)…"
    planEnv <- newBuildEnv console 1 []
    okPlan <- runCabal planEnv "plan" (["build", "--dry-run"] ++ dryTargets ++ cabalOpts)
    unless okPlan $ die console "ffcabal: cabal build --dry-run failed"
    units <- readPlan planDir >>= either (die console . T.pack) return
    let locals = [ u | u <- units, isLocalUnit u, isCheckableComponent u ]

    -- 2. Select + order.
    selected <- case oTargets opts of
        [] -> return [ u | u <- locals, not (isTestBench u) ]
        ts -> case resolveTargets locals ts of
            Left err     -> die console ("ffcabal: " <> err)
            Right starts -> return (reachable locals starts)
    let ordered = topoOrder selected
        byId = M.fromList [ (puId u, u) | u <- ordered ]
        localDepIds u = map puId (localDeps byId u)
    when (null ordered) $ die console "ffcabal: nothing to do (no local components selected)"
    cOut console $ "ffcabal: " <> T.pack (show (length ordered))
                   <> " component(s), dependency order: "
                   <> T.intercalate ", " (map unitTarget ordered)

    case oCmd opts of
      CmdBuild -> do
        jobsDefault <- max 1 . min 4 <$> getNumProcessors
        env <- newBuildEnv console (fromMaybe jobsDefault (oJobs opts)) (map puId ordered)
        -- Interrupt safety: our child `cabal build` jobs run in their own
        -- process groups (so we can cancel them individually), which means a
        -- group signal aimed at ffcabal (e.g. leksah interrupting a build)
        -- does NOT reach them — reap them ourselves, then exit.  The tmux
        -- repls are unaffected (they live in the tmux server).
        let bailOut = failNow env >> exitImmediately (ExitFailure 130)
        _ <- installHandler sigINT  (Catch bailOut) Nothing
        _ <- installHandler sigTERM (Catch bailOut) Nothing
        let loop [] = return True
            loop (u : us) = do
                bail <- anyFailed env
                if bail then return False else do
                    -- deps' real builds must be done so the repl builds nothing unchecked
                    depsOk <- if oReplOnly opts then return True
                              else and <$> mapM (waitUnit env) (localDepIds u)
                    if not depsOk then return False else do
                        r <- checkUnitRepl console cwd stateDir envFile cabalOpts (oTimeout opts) u
                        emitSegment console r
                        case rrOutcome r of
                          ReplOk -> do
                              cOut console $ "ffcabal: ✓ " <> unitTarget u
                                             <> (if rrReused r then " (cached repl)" else "")
                              unless (oReplOnly opts) $ spawnBuild env cabalOpts u (localDepIds u)
                              loop us
                          bad -> do
                              cErr console $ "ffcabal: ✗ " <> unitTarget u <> " — " <> describeBad bad
                              failNow env
                              return False
        checksOk <- loop ordered
        buildsOk <- if oReplOnly opts then return True else waitAllBuilds env
        unless (checksOk && buildsOk) $ exitWith (ExitFailure 1)
        if oReplOnly opts
          then cOut console "ffcabal: all checks passed (checks only; nothing built)"
          else do
            cOut console "ffcabal: all checks passed — final cabal build"
            okF <- runCabal env "final" (["build"] ++ dryTargets ++ cabalOpts)
            unless okF $ exitWith (ExitFailure 1)
            cOut console "ffcabal: OK"

      cmd -> do
        when (null (oTargets opts)) $
            die console "ffcabal: repl/check need exactly one TARGET"
        u <- case resolveTargets locals (oTargets opts) of
            Right [one]   -> return one
            Right several -> die console $ "ffcabal: target matches several components ("
                <> T.intercalate ", " (map unitTarget several) <> "); pick one"
            Left err      -> die console ("ffcabal: " <> err)
        r <- checkUnitRepl console cwd stateDir envFile cabalOpts (oTimeout opts) u
        emitSegment console r
        case rrOutcome r of
          ReplOk -> do
              when (cmd == CmdRepl) $
                  cOut console $ "ffcabal: repl ready — attach: tmux attach -t ffcabal   (window '"
                                 <> T.pack (rrWindow r) <> "')"
              cOut console $ "ffcabal: ✓ " <> unitTarget u
          bad -> do
              cErr console $ "ffcabal: ✗ " <> unitTarget u <> " — " <> describeBad bad
              exitWith (ExitFailure 1)
  where
    isTestBench u = maybe False (\c -> "test:" `T.isPrefixOf` c || "bench:" `T.isPrefixOf` c)
                          (puComponent u)
    -- GHC diagnostics go to stderr even when the check passed — warnings in
    -- an Ok segment must reach stderr-parsing consumers (Leksah's Errors
    -- pane).  Died/timed-out segments are cabal/tmux failure text, not GHC
    -- output: keep the whole segment on stderr.
    emitSegment console r
      | rrOutcome r `elem` [ReplOk, ReplFailed] =
          mapM_ (\(diag, l) -> (if diag then cErr else cOut) console l)
                (classifyDiagnostics (T.lines (rrOutput r)))
      | otherwise = mapM_ (cErr console) (T.lines (rrOutput r))
    describeBad ReplFailed  = "type errors (see above)"
    describeBad ReplDied    = "the repl exited (cabal error? see above / the tmux window)"
    describeBad ReplTimeout = "timed out waiting for the repl (busy? attach: tmux attach -t ffcabal)"
    describeBad ReplOk      = "ok"

die :: Console -> Text -> IO a
die console msg = do
    cErr console msg
    exitWith (ExitFailure 1)

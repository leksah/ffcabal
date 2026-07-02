{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | The fail-fast probe: type-check a component in a cached @cabal repl@.
--
-- Each component gets a tmux window (session @ffcabal@) running
-- @cabal repl \<unit\>@.  A check is:
--
--   * reuse the window when its recorded UnitId still matches the plan
--     (same flags\/deps), sending @:reload@ — usually seconds;
--   * otherwise (re)spawn the window with a fresh repl and wait for the
--     initial load.
--
-- Output is captured with @tmux pipe-pane@.  Command fencing: after the
-- command we type @:!echo FFCABAL_SY\"\"NC_\<nonce\>@ — GHCi runs commands
-- sequentially and a PTY is one ordered byte stream, so everything captured
-- before the nonce line is the command's complete output (stdout and stderr
-- already interleaved in write order).  This replaces the stderr-sentinel
-- handshake Leksah's @IDE.Utils.Tool@ needs when driving ghci over separate
-- racing pipes.  The quote-split in the echo keeps the *typed* (tty-echoed)
-- command line from matching the nonce we scan for.
--
-- The verdict comes from GHCi itself: the last @Ok, … loaded.@ /
-- @Failed, … loaded.@ line of the captured segment.
module FFCabal.Repl
  ( ReplOutcome(..)
  , ReplResult(..)
  , checkUnitRepl
  , writeEnvFile
  -- * exposed for the test suite
  , lastVerdict
  , nonceMatchesLine
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnvironment)
import System.FilePath ((</>), isAbsolute, takeDirectory)
import System.IO
       (IOMode(ReadMode), SeekMode(AbsoluteSeek), hFileSize, hSeek,
        withBinaryFile)
import System.Posix.Process (getProcessID)

import FFCabal.Output (Console, cOut, stripAnsi)
import FFCabal.Plan (PlanUnit(..), unitTarget)
import FFCabal.Tmux

data ReplOutcome
  = ReplOk        -- ^ GHCi said @Ok, … loaded.@
  | ReplFailed    -- ^ GHCi said @Failed, … loaded.@ (or no verdict)
  | ReplDied      -- ^ the pane died (e.g. @cabal repl@ itself errored)
  | ReplTimeout   -- ^ no nonce within the timeout (repl busy or wedged?)
  deriving (Eq, Show)

data ReplResult = ReplResult
  { rrOutcome :: ReplOutcome
  , rrOutput  :: Text     -- ^ the captured segment (ANSI-stripped)
  , rrReused  :: Bool     -- ^ True = @:reload@ in an existing repl
  , rrWindow  :: String   -- ^ tmux window name (attach hint)
  } deriving Show

-- | Check one component.  @cabalOpts@ must already include any @--builddir@.
checkUnitRepl
  :: Console
  -> FilePath      -- ^ project root (cwd for the repl)
  -> FilePath      -- ^ state dir for logs
  -> FilePath      -- ^ env file (see 'writeEnvFile')
  -> [String]      -- ^ extra cabal options (passed to @cabal repl@)
  -> Int           -- ^ timeout (seconds)
  -> PlanUnit
  -> IO ReplResult
checkUnitRepl console projRoot stateDir envFile cabalOpts timeoutSecs u = do
    createDirectoryIfMissing True stateDir
    let target  = unitTarget u
        name    = T.unpack target
        logFile = stateDir </> sanitize name <> ".log"
        -- Source the captured environment so cabal inside tmux sees the same
        -- configuration (PATH!) as the cabal that planned the build.
        replCmd = ". " <> shQuote envFile <> " ; exec cabal repl "
                  <> unwords (map shQuote (name : "--repl-options=-ferror-spans" : cabalOpts))
    wins <- listReplWindows
    let mine = find (\w -> winDir w == projRoot && winName w == name) wins
    case mine of
      Just w | not (winDead w), winUnitId w == T.unpack (puId u) -> do
        -- Reuse: interrupt anything running / clear a half-typed line, :reload.
        cOut console $ "ffcabal: checking " <> target <> " (:reload in cached repl)"
        let pane = winPane w
            -- only trust a recorded log path that is absolute (older versions
            -- recorded relative paths, which pipe-pane resolves elsewhere)
            logF = if isAbsolute (winLog w) then winLog w else logFile
        sendCancelCopyMode pane
        sendNamedKey pane "C-c"      -- interrupt anything running
        threadDelay 150000
        sendNamedKey pane "C-u"      -- clear a half-typed line
        when (logF /= winLog w) $ setWinOption (winId w) "@ffcabal_log" logF
        off <- fileSize logF
        pipePane pane logF
        sendKeys pane ":reload"
        finishCheck console True name pane logF off timeoutSecs
      Just w -> do
        -- Stale (UnitId changed) or dead: restart the repl in the same window.
        cOut console $ "ffcabal: checking " <> target <> " (repl restarted: "
                       <> (if winDead w then "previous repl exited" else "configuration changed") <> ")"
        off <- fileSize logFile
        respawnReplWindow (winId w) projRoot replCmd >>= \case
          Nothing -> return $ ReplResult ReplDied "ffcabal: tmux respawn-window failed" False name
          Just pane -> do
            recordIdentity (winId w) logFile
            pipePane pane logFile
            -- the nonce (sent by finishCheck) queues in the tty and runs
            -- once ghci finishes its initial load
            finishCheck console False name pane logFile off timeoutSecs
      Nothing -> do
        cOut console $ "ffcabal: checking " <> target <> " (starting repl in tmux; attach: tmux attach -t "
                       <> T.pack sessionName <> ")"
        off <- fileSize logFile
        newReplWindow name projRoot replCmd >>= \case
          Nothing -> return $ ReplResult ReplDied "ffcabal: tmux new-window failed" False name
          Just (wid, pane) -> do
            recordIdentity wid logFile
            pipePane pane logFile
            finishCheck console False name pane logFile off timeoutSecs
  where
    recordIdentity wid logFile = do
        setWinOption wid "@ffcabal_unitid" (T.unpack (puId u))
        setWinOption wid "@ffcabal_dir" projRoot
        setWinOption wid "@ffcabal_log" logFile

-- | Send the nonce fence, wait for it (or death/timeout), extract the
-- segment and the GHCi verdict.
finishCheck :: Console -> Bool -> String -> String -> FilePath -> Integer -> Int -> IO ReplResult
finishCheck _console reused name pane logF off timeoutSecs = do
    nonce <- newNonce
    -- The quote-split keeps the echoed *input* from containing the nonce.
    sendKeys pane (":!echo " <> T.replace "SYNC" "SY\"\"NC" nonce)
    result <- waitForNonce pane logF off nonce (timeoutSecs * 10)
    case result of
      WaitFound segment ->
        let out = stripAnsi segment
            verdict = lastVerdict out
        in return $ ReplResult (fromMaybe ReplFailed verdict) out reused name
      WaitDied segment ->
        return $ ReplResult ReplDied (stripAnsi segment) reused name
      WaitTimeout segment ->
        return $ ReplResult ReplTimeout (stripAnsi segment) reused name

data WaitResult = WaitFound Text | WaitDied Text | WaitTimeout Text

-- | Does a captured (ANSI-stripped) line mark the nonce fence?  Suffix match:
-- exotic terminal sequences can fuse the nonce onto a previous line; the
-- *typed* echo can never end with the real nonce (the SY\"\"NC quote-split
-- breaks it), so a suffix match is still unambiguous.
nonceMatchesLine :: Text -> Text -> Bool
nonceMatchesLine nonce l = nonce `T.isSuffixOf` T.strip l

-- | Poll the pipe-pane log (from @off@) until a line equals the nonce.
-- Ticks are 100 ms; @maxTicks@ bounds the wait.  A dead pane ends the wait
-- (after a short grace period so the pipe flushes).
waitForNonce :: String -> FilePath -> Integer -> Text -> Int -> IO WaitResult
waitForNonce pane logF off nonce maxTicks = go (0 :: Int)
  where
    go tick = do
        content <- readFrom logF off
        let plain = stripAnsi content
            lns   = T.lines plain
        case break (nonceMatchesLine nonce) lns of
          -- Include the (nonce-stripped) boundary line: a verdict can fuse
          -- onto it when output and the echoed nonce interleave.
          (before, m : _) ->
              let m' = T.strip (T.replace nonce "" m)
              in return $ WaitFound (T.unlines (before ++ [m' | not (T.null m')]))
          _ | tick >= maxTicks -> return $ WaitTimeout plain
            | otherwise -> do
                dead <- if tick `mod` 5 == 0 then paneDead pane else return False
                if dead
                  then do
                    threadDelay 300000   -- let pipe-pane flush
                    finalContent <- readFrom logF off
                    return $ WaitDied (stripAnsi finalContent)
                  else do
                    threadDelay 100000
                    go (tick + 1)

-- | The last @Ok,@ / @Failed,@ … @loaded.@ line decides the verdict.
-- Matched as infix + suffix (not prefix): when a command is typed while the
-- previous one still runs, its tty echo can fuse onto the front of the
-- verdict line (e.g. @:!echo …SY""NC_…Ok, one module reloaded.@).
lastVerdict :: Text -> Maybe ReplOutcome
lastVerdict t = go Nothing (T.lines t)
  where
    go acc [] = acc
    go acc (l:ls)
      | isVerdict "Failed," l = go (Just ReplFailed) ls
      | isVerdict "Ok," l     = go (Just ReplOk) ls
      | otherwise             = go acc ls
    isVerdict p l = let s = T.strip l
                    in "loaded." `T.isSuffixOf` s && p `T.isInfixOf` s

newNonce :: IO Text
newNonce = do
    pid <- getProcessID
    now <- getPOSIXTime
    return $ "FFCABAL_SYNC_" <> T.pack (show pid) <> "_"
             <> T.pack (show (floor (now * 1000) :: Integer))

fileSize :: FilePath -> IO Integer
fileSize f = doesFileExist f >>= \case
    False -> return 0
    True  -> withBinaryFile f ReadMode hFileSize

-- | Read the file from a byte offset (UTF-8, lenient — it's a PTY capture).
readFrom :: FilePath -> Integer -> IO Text
readFrom f off = doesFileExist f >>= \case
    False -> return ""
    True  -> withBinaryFile f ReadMode $ \h -> do
        size <- hFileSize h
        if size <= off then return "" else do
            hSeek h AbsoluteSeek off
            TE.decodeUtf8With TE.lenientDecode <$> BS.hGet h (fromInteger (size - off))

-- | Capture ffcabal's own environment as a sourceable shell script, so the
-- repl started inside tmux runs with an identical configuration (cabal treats
-- a different environment — notably PATH — as "configuration changed").
writeEnvFile :: FilePath -> IO ()
writeEnvFile path = do
    createDirectoryIfMissing True (takeDirectory path)
    env <- getEnvironment
    writeFile path . unlines $
        [ "export " <> k <> "=" <> shQuote v
        | (k, v) <- env
        , validName k
        , k `notElem` excluded ]
  where
    excluded = ["TMUX", "TMUX_PANE", "TERM", "PWD", "OLDPWD", "SHLVL", "_"]
    validName [] = False
    validName (c:cs) = startOk c && all restOk cs
    startOk c = isAsciiLower c || isAsciiUpper c || c == '_'
    restOk c = isAlphaNum c || c == '_'

sanitize :: String -> String
sanitize = map (\c -> if isAlphaNum c || c `elem` ("-._" :: String) then c else '_')

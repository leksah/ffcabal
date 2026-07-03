{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | tmux plumbing for the repl windows.  Everything runs on the user's
-- *default* tmux server, in a session named @ffcabal@, one window per
-- component.  Window user options carry the identity ffcabal needs to decide
-- reuse:
--
--   [@\@ffcabal_unitid@]   the plan UnitId the repl was started for
--   [@\@ffcabal_dir@]      the project root
--   [@\@ffcabal_log@]      the pipe-pane capture file
--   [@\@ffcabal_confhash@] the project config hash (see 'FFCabal.Plan.configHash')
--
-- All tmux calls are argv-level ('readProcessWithExitCode'), so names never
-- pass through a shell; only the pane's own command line is shell-quoted.
module FFCabal.Tmux
  ( Win(..)
  , sessionName
  , ensureSession
  , listReplWindows
  , newReplWindow
  , respawnReplWindow
  , killWindow
  , setWinOption
  , pipePane
  , sendKeys
  , sendNamedKey
  , sendCancelCopyMode
  , paneDead
  , paneInMode
  , shQuote
  ) where

import Control.Monad (unless, void, when)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

sessionName :: String
sessionName = "ffcabal"

-- | Every tmux invocation is prefixed with the (word-split) contents of the
-- @FFCABAL_TMUX_ARGS@ environment variable, so tests — or a cautious user —
-- can redirect ffcabal's repls to a scratch server, e.g.
-- @FFCABAL_TMUX_ARGS=\"-L my-socket\"@.  Unset\/empty = the default server.
tmux :: [String] -> IO (ExitCode, String, String)
tmux args = do
    extra <- maybe [] words <$> lookupEnv "FFCABAL_TMUX_ARGS"
    readProcessWithExitCode "tmux" (extra ++ args) ""

tmuxOk :: [String] -> IO Bool
tmuxOk args = (\(c, _, _) -> c == ExitSuccess) <$> tmux args

tmuxOut :: [String] -> IO (Maybe String)
tmuxOut args = tmux args >>= \case
    (ExitSuccess, out, _) -> return (Just (trim out))
    _                     -> return Nothing
  where trim = reverse . dropWhile (`elem` ("\r\n " :: String)) . reverse

-- | Make sure the @ffcabal@ session exists (detached; a plain shell in
-- window 0 so the session survives repl windows closing).
ensureSession :: IO ()
ensureSession = do
    have <- tmuxOk ["has-session", "-t", "=" <> sessionName]
    unless have . void $
        tmux ["new-session", "-d", "-s", sessionName]

data Win = Win
  { winId       :: String   -- ^ @\@n@
  , winPane     :: String   -- ^ @%n@
  , winName     :: String
  , winDead     :: Bool
  , winUnitId   :: String   -- ^ @\@ffcabal_unitid@ ("" if unset)
  , winDir      :: String   -- ^ @\@ffcabal_dir@ ("" if unset)
  , winLog      :: String   -- ^ @\@ffcabal_log@ ("" if unset)
  , winConfHash :: String   -- ^ @\@ffcabal_confhash@ ("" if unset)
  } deriving Show

-- | The ffcabal session's windows (with our identity options).
listReplWindows :: IO [Win]
listReplWindows =
    tmuxOut [ "list-windows", "-t", "=" <> sessionName, "-F"
            , intercalate "\t"
                [ "#{window_id}", "#{pane_id}", "#{pane_dead}"
                , "#{@ffcabal_unitid}", "#{@ffcabal_dir}", "#{@ffcabal_log}"
                , "#{@ffcabal_confhash}", "#{window_name}" ] ] >>= \case
      Nothing  -> return []
      Just out -> return . mapMaybe parse $ lines out
  where
    parse l = case splitOn '\t' l of
        (wid : pane : dead : unit : dir : logf : ch : nameParts) ->
            Just Win { winId = wid, winPane = pane, winDead = dead == "1"
                     , winUnitId = unit, winDir = dir, winLog = logf
                     , winConfHash = ch
                     , winName = intercalate "\t" nameParts }
        _ -> Nothing
    splitOn c s = case break (== c) s of
        (a, _ : rest) -> a : splitOn c rest
        (a, [])       -> [a]

-- | Create a new repl window running @cmd@ (a shell command line) in @cwd@.
-- Returns (window-id, pane-id).  @remain-on-exit@ is turned on so a failed
-- @cabal repl@ leaves its output readable instead of vanishing.
newReplWindow :: String -> FilePath -> String -> IO (Maybe (String, String))
newReplWindow name cwd cmd = do
    ensureSession
    r <- tmuxOut [ "new-window", "-d", "-t", "=" <> sessionName <> ":"
                 , "-n", name, "-c", cwd
                 , "-P", "-F", "#{window_id}\t#{pane_id}", cmd ]
    case r >>= pair of
        Nothing -> return Nothing
        Just (wid, pane) -> do
            void $ tmux ["set-option", "-w", "-t", wid, "remain-on-exit", "on"]
            return (Just (wid, pane))
  where
    pair s = case break (== '\t') s of
        (a, '\t' : b) -> Just (a, b)
        _             -> Nothing

-- | Restart a (dead or stale) repl window in place with a new command.
-- Returns the (possibly new) pane id.
respawnReplWindow :: String -> FilePath -> String -> IO (Maybe String)
respawnReplWindow wid cwd cmd = do
    ok <- tmuxOk ["respawn-window", "-k", "-t", wid, "-c", cwd, cmd]
    if ok
      then tmuxOut ["display-message", "-p", "-t", wid, "#{pane_id}"]
      else return Nothing

killWindow :: String -> IO ()
killWindow wid = void $ tmux ["kill-window", "-t", wid]

setWinOption :: String -> String -> String -> IO ()
setWinOption wid opt val = void $ tmux ["set-option", "-w", "-t", wid, opt, val]

-- | Capture the pane's output (append) into @logFile@ — only if no pipe is
-- already open.  NB @pipe-pane -o@ is a *toggle* (an existing pipe would be
-- closed!), so we must check @pane_pipe@ ourselves instead of re-invoking it.
pipePane :: String -> FilePath -> IO ()
pipePane pane logFile = do
    piped <- (== Just "1") <$> tmuxOut ["display-message", "-p", "-t", pane, "#{pane_pipe}"]
    unless piped . void $
        tmux ["pipe-pane", "-o", "-t", pane, "cat >> " <> shQuote logFile]

-- | Type text into the pane.  Uses @-l@ (literal) for the text and a separate
-- @Enter@ key so tmux key-name lookup can't mangle the content.
sendKeys :: String -> Text -> IO ()
sendKeys pane t = do
    void $ tmux ["send-keys", "-t", pane, "-l", T.unpack t]
    void $ tmux ["send-keys", "-t", pane, "Enter"]

-- | Send a named key (e.g. @C-c@, @C-u@) — NOT literal.
sendNamedKey :: String -> String -> IO ()
sendNamedKey pane key = void $ tmux ["send-keys", "-t", pane, key]

-- | If the pane is in copy-mode (the user was scrolling), leave it so typed
-- keys reach the program.
sendCancelCopyMode :: String -> IO ()
sendCancelCopyMode pane = do
    inMode <- paneInMode pane
    when inMode . void $ tmux ["send-keys", "-t", pane, "-X", "cancel"]

paneDead :: String -> IO Bool
paneDead pane =
    (== Just "1") <$> tmuxOut ["display-message", "-p", "-t", pane, "#{pane_dead}"]

paneInMode :: String -> IO Bool
paneInMode pane =
    (== Just "1") <$> tmuxOut ["display-message", "-p", "-t", pane, "#{pane_in_mode}"]

-- | POSIX single-quote shell escaping.
shQuote :: String -> String
shQuote s = "'" <> concatMap esc s <> "'"
  where esc '\'' = "'\\''"
        esc c    = [c]

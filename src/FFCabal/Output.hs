{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | A single-writer console.
--
-- ffcabal streams the output of several concurrent subprocesses (parallel
-- @cabal build@ jobs, each with a stdout and a stderr reader thread).  If
-- every thread wrote to the terminal directly, lines would interleave and
-- the compiler errors on stderr could be garbled mid-line — the problem
-- Leksah's @IDE.Utils.Tool@ solves by funnelling all reader threads into one
-- ordered channel.  We do the same: every line is sent (tagged out/err) to a
-- channel drained by one writer thread that owns both handles.
module FFCabal.Output
  ( Console
  , withConsole
  , cOut
  , cErr
  , stripAnsi
  , classifyDiagnostics
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
       (BufferMode(LineBuffering), hSetBuffering, stderr, stdout)

data Msg = MsgOut Text | MsgErr Text | MsgClose

newtype Console = Console (Chan Msg)

-- | Run an action with a console; all queued lines are flushed before return.
withConsole :: (Console -> IO a) -> IO a
withConsole body = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    chan <- newChan
    done <- newEmptyMVar :: IO (MVar ())
    _ <- forkIO (writer chan done)
    body (Console chan) `finally` (writeChan chan MsgClose >> takeMVar done)
  where
    writer chan done = readChan chan >>= \case
        MsgOut t -> T.hPutStrLn stdout t >> writer chan done
        MsgErr t -> T.hPutStrLn stderr t >> writer chan done
        MsgClose -> putMVar done ()

-- | Queue a line for stdout / stderr (safe from any thread).
cOut, cErr :: Console -> Text -> IO ()
cOut (Console chan) = writeChan chan . MsgOut
cErr (Console chan) = writeChan chan . MsgErr

-- | Split a captured repl segment into GHC diagnostics and chatter.  GHC
-- writes diagnostics to stderr, but a PTY capture merges the streams, so we
-- re-split by shape: a diagnostic starts with a @file:span: error:/warning:@
-- header and continues over indented lines (including the @NN | code@
-- gutter) until the first non-indented line.  'True' = diagnostic — emit it
-- on stderr, where build tools (and Leksah's error parser) expect compiler
-- diagnostics; chatter (ghci banner, echoed commands, @Ok, … loaded.@) stays
-- on stdout so it can't be mistaken for part of a diagnostic.
classifyDiagnostics :: [Text] -> [(Bool, Text)]
classifyDiagnostics = go False
  where
    go _ [] = []
    go inDiag (l : ls)
      | isHeader l         = (True, l)  : go True ls
      | inDiag && isCont l = (True, l)  : go True ls
      | otherwise          = (False, l) : go False ls
    isHeader l =
        not (" " `T.isPrefixOf` l)
        && any (`T.isInfixOf` l) [": error:", ": warning:", ": error [", ": warning ["]
    -- Continuation: indented message/caret lines, the code line of the
    -- gutter (@10 | …@ — starts with the line number, not a space), and the
    -- blank line GHC prints between diagnostics.
    isCont l = T.null (T.strip l) || " " `T.isPrefixOf` l || isGutter l
    isGutter l = case T.span isDigit l of
        (d, r) -> not (T.null d) && " |" `T.isPrefixOf` r

-- | Drop ANSI escape sequences (CSI and OSC) and normalise carriage returns —
-- tmux @pipe-pane@ captures the raw PTY byte stream, including GHC's colours.
-- A bare @\\r@ acts as a line break: raw terminal output ends lines with
-- @\\r\\n@, but line-editors (haskeline) sometimes emit a lone @\\r@ before the
-- next program writes — deleting it would fuse two logical lines.
stripAnsi :: Text -> Text
stripAnsi = T.pack . go . T.unpack
  where
    go [] = []
    go ('\r':'\n':rest) = '\n' : go rest
    go ('\r':rest) = '\n' : go rest
    go ('\ESC':'[':rest) = go (drop 1 (dropWhile csiParam rest))
    go ('\ESC':']':rest) = go (dropOsc rest)
    -- NEL (next line) and IND (index): cursor-to-next-line controls that
    -- line editors emit instead of \r\n — they ARE line breaks.
    go ('\ESC':'E':rest) = '\n' : go rest
    go ('\ESC':'D':rest) = '\n' : go rest
    go ('\ESC':_:rest) = go rest
    go (c:rest) = c : go rest
    -- CSI parameter/intermediate bytes: 0x20-0x3F; final byte 0x40-0x7E ends it.
    csiParam c = c >= ' ' && c <= '?' || isDigit c
    -- OSC ends with BEL or ST (ESC \)
    dropOsc ('\a':rest) = rest
    dropOsc ('\ESC':'\\':rest) = rest
    dropOsc (_:rest) = dropOsc rest
    dropOsc [] = []

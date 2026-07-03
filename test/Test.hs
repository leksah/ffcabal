{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | ffcabal test suite: pure unit tests for the plan\/graph\/output\/repl
-- logic, plus guarded integration tests that drive the real @ffcabal@ binary
-- against a generated two-package fixture project on a THROWAWAY tmux server
-- (@FFCABAL_TMUX_ARGS=\"-L ffcabal-test-<pid>\"@ — never the user's default
-- server, never leksah's socket).  Integration is skipped (not failed) when
-- ghc\/cabal\/tmux or a built ffcabal binary is unavailable.
--
-- Hand-rolled PASS\/FAIL assertions in the style of scripts\/tmux-cc-test.hs;
-- zero dependencies beyond ffcabal's own.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, finally, try)
import Control.Monad (unless, void)
import Data.IORef
import Data.List (isInfixOf, sort)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
       (createDirectoryIfMissing, doesFileExist, findExecutable,
        getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)
import System.Posix.User (getEffectiveUserID)
import System.Process
       (CreateProcess(cwd, env), proc, readCreateProcessWithExitCode,
        readProcessWithExitCode)

import FFCabal.Graph
       (localDeps, reachable, resolveTargets, topoOrder)
import FFCabal.Output (classifyDiagnostics, stripAnsi)
import FFCabal.Plan
       (PlanUnit(..), isCheckableComponent, isLocalUnit, readPlan, unitTarget)
import FFCabal.Repl (ReplOutcome(..), lastVerdict, nonceMatchesLine)
import FFCabal.Tmux (shQuote)

main :: IO ()
main = do
    failures <- newIORef (0 :: Int)
    let check name ok = do
            putStrLn $ (if ok then "PASS " else "FAIL ") <> name
            unless ok $ modifyIORef' failures (+ 1)
        checkEq :: (Eq a, Show a) => String -> a -> a -> IO ()
        checkEq name actual expected = do
            let ok = actual == expected
            putStrLn $ (if ok then "PASS " else "FAIL ") <> name
            unless ok $ do
                putStrLn $ "  expected: " <> show expected
                putStrLn $ "  actual:   " <> show actual
                modifyIORef' failures (+ 1)

    putStrLn "== FFCabal.Output.stripAnsi =="
    checkEq "plain text untouched" (stripAnsi "hello world") "hello world"
    checkEq "blank lines preserved" (stripAnsi "a\n\nb") "a\n\nb"
    checkEq "CSI stripped" (stripAnsi (esc "[31m" <> "red" <> esc "[0m" <> "!")) "red!"
    checkEq "CSI with params" (stripAnsi (esc "[1;38;5;196m" <> "x")) "x"
    checkEq "OSC to BEL stripped" (stripAnsi ("a" <> esc "]0;title\a" <> "b")) "ab"
    checkEq "OSC to ST stripped" (stripAnsi ("a" <> esc "]8;;http://x" <> esc "\\" <> "b")) "ab"
    checkEq "crlf -> newline" (stripAnsi "a\r\nb") "a\nb"
    checkEq "lone cr -> newline" (stripAnsi "a\rb") "a\nb"
    checkEq "NEL (ESC E) -> newline" (stripAnsi ("a" <> esc "E" <> "b")) "a\nb"
    checkEq "IND (ESC D) -> newline" (stripAnsi ("a" <> esc "D" <> "b")) "a\nb"
    checkEq "other ESC x dropped" (stripAnsi ("a" <> esc "=" <> "b")) "ab"

    putStrLn "== FFCabal.Output.classifyDiagnostics =="
    let diags = map fst . classifyDiagnostics
    checkEq "chatter only" (diags ["GHCi, version 9.14.1", "ghci> :reload", "Ok, 27 modules reloaded."])
        [False, False, False]
    checkEq "warning block to stderr, chatter around it stays out"
        (classifyDiagnostics
            [ "[ 3 of 27] Compiling IDE.Foo ( src/IDE/Foo.hs, interpreted )"
            , "src/IDE/Foo.hs:10:1: warning: [-Wunused-imports]"
            , "    The import of \8216Data.Maybe\8217 is redundant"
            , "   |"
            , "10 | import Data.Maybe"
            , "   | ^^^^^^^^^^^^^^^^^"
            , ""
            , "Ok, 27 modules reloaded."
            ])
        [ (False, "[ 3 of 27] Compiling IDE.Foo ( src/IDE/Foo.hs, interpreted )")
        , (True,  "src/IDE/Foo.hs:10:1: warning: [-Wunused-imports]")
        , (True,  "    The import of \8216Data.Maybe\8217 is redundant")
        , (True,  "   |")
        , (True,  "10 | import Data.Maybe")
        , (True,  "   | ^^^^^^^^^^^^^^^^^")
        , (True,  "")
        , (False, "Ok, 27 modules reloaded.")
        ]
    checkEq "error header (with GHC error code) starts a block"
        (diags ["src/A.hs:2:5: error: [GHC-83865]", "    Couldn't match", "done"])
        [True, True, False]
    checkEq "no-location error header"
        (diags ["<no location info>: error:", "    ghc bug?", "next"])
        [True, True, False]
    checkEq "blank line inside a block does not end it"
        (diags ["a.hs:1:1: warning: x", "", "  more", "Ok, one module loaded."])
        [True, True, True, False]
    checkEq "indented line without a header is chatter"
        (diags ["  just indented banner text"]) [False]

    putStrLn "== FFCabal.Tmux.shQuote =="
    checkEq "simple" (shQuote "abc") "'abc'"
    checkEq "embedded quote" (shQuote "a'b") "'a'\\''b'"
    checkEq "empty" (shQuote "") "''"

    putStrLn "== FFCabal.Repl verdicts =="
    checkEq "Ok verdict" (lastVerdict "Ok, one module loaded.") (Just ReplOk)
    checkEq "Ok reloaded" (lastVerdict "Ok, 19 modules reloaded.") (Just ReplOk)
    checkEq "Failed verdict" (lastVerdict "Failed, no modules to be reloaded.") (Just ReplFailed)
    checkEq "last one wins (Ok then Failed)"
        (lastVerdict "Ok, one module loaded.\nsome error\nFailed, no modules to be reloaded.")
        (Just ReplFailed)
    checkEq "last one wins (Failed then Ok)"
        (lastVerdict "Failed, no modules to be reloaded.\nfixed\nOk, one module reloaded.")
        (Just ReplOk)
    checkEq "fused echo verdict"
        (lastVerdict ":!echo FFCABAL_SY\"\"NC_1Ok, one module reloaded.")
        (Just ReplOk)
    checkEq "no verdict" (lastVerdict "just some noise\nCompiling Foo") Nothing
    checkEq "prompt-prefixed verdict"
        (lastVerdict "ghci> Ok, one module loaded.") (Just ReplOk)

    putStrLn "== FFCabal.Repl nonce discipline =="
    let nonce = "FFCABAL_SYNC_42_1234"
    check "real output line matches" (nonceMatchesLine nonce nonce)
    check "padded output line matches" (nonceMatchesLine nonce ("  " <> nonce <> "  "))
    check "fused echo+output matches"
        (nonceMatchesLine nonce ("ghci> :!echo FFCABAL_SY\"\"NC_42_1234" <> nonce))
    check "typed echo does NOT match"
        (not (nonceMatchesLine nonce ":!echo FFCABAL_SY\"\"NC_42_1234"))
    check "prompted typed echo does NOT match"
        (not (nonceMatchesLine nonce "ghci> :!echo FFCABAL_SY\"\"NC_42_1234"))
    check "unrelated line does NOT match" (not (nonceMatchesLine nonce "Ok, loaded."))

    putStrLn "== FFCabal.Plan (fixture plan.json) =="
    tmpRoot <- getTemporaryDirectory
    pid <- getProcessID
    let planDir = tmpRoot </> ("ffcabal-test-plan-" <> show pid)
    createDirectoryIfMissing True (planDir </> "cache")
    writeFile (planDir </> "cache" </> "plan.json") fixturePlanJson
    plan <- readPlan planDir
    case plan of
      Left err -> check ("plan.json parses: " <> err) False
      Right units -> do
        check "plan.json parses" True
        checkEq "unit count" (length units) 10
        let locals = filter isLocalUnit units
            checkables = filter isCheckableComponent locals
            byTarget t = [ u | u <- units, unitTarget u == t ]
        checkEq "local unit ids" (sort (map puId locals))
            (sort [ "ltk-0.1.0.0-inplace", "vcswrapper-0.1.0.0-inplace"
                  , "leksah-server-0.17.0.0-inplace", "leksah-0.17.0.0-inplace-leksah-nogtk"
                  , "leksah-0.17.0.0-inplace-leksah-wkwebview", "leksah-0.17.0.0-inplace-setup"
                  , "leksah-server-0.17.0.0-inplace-test-tool", "odd-0.1-inplace" ])
        checkEq "checkable excludes setup and missing component-name"
            (sort (map puId checkables))
            (sort [ "ltk-0.1.0.0-inplace", "vcswrapper-0.1.0.0-inplace"
                  , "leksah-server-0.17.0.0-inplace", "leksah-0.17.0.0-inplace-leksah-nogtk"
                  , "leksah-0.17.0.0-inplace-leksah-wkwebview"
                  , "leksah-server-0.17.0.0-inplace-test-tool" ])
        checkEq "unitTarget lib" (map unitTarget (byTarget "ltk:lib:ltk")) ["ltk:lib:ltk"]
        checkEq "unitTarget sublib"
            (map puId (byTarget "leksah:lib:leksah-nogtk")) ["leksah-0.17.0.0-inplace-leksah-nogtk"]
        checkEq "unitTarget exe"
            (map puId (byTarget "leksah:exe:leksah-wkwebview")) ["leksah-0.17.0.0-inplace-leksah-wkwebview"]
        check "pre-existing base not local" $
            not (any isLocalUnit [ u | u <- units, puId u == "base-4.22.0.0-inplace" ])
        check "global configured dep not local" $
            not (any isLocalUnit [ u | u <- units, puId u == "sn-2.2.5.0-61efbe57" ])

        putStrLn "== FFCabal.Graph (fixture + hand graphs) =="
        let byId us = M.fromList [ (puId u, u) | u <- us ]
        case (byTarget "leksah:lib:leksah-nogtk", byTarget "leksah:exe:leksah-wkwebview") of
          ([nogtk], [wk]) -> do
            checkEq "localDeps of nogtk (restricted to local set)"
                (sort (map puId (localDeps (byId checkables) nogtk)))
                (sort [ "ltk-0.1.0.0-inplace", "vcswrapper-0.1.0.0-inplace"
                      , "leksah-server-0.17.0.0-inplace" ])
            checkEq "reachable from exe"
                (sort (map puId (reachable checkables [wk])))
                (sort [ "leksah-0.17.0.0-inplace-leksah-wkwebview"
                      , "leksah-0.17.0.0-inplace-leksah-nogtk"
                      , "leksah-server-0.17.0.0-inplace"
                      , "ltk-0.1.0.0-inplace", "vcswrapper-0.1.0.0-inplace" ])
            check "topoOrder deps-first (fixture)" (depsFirst (topoOrder (reachable checkables [wk])))
          _ -> check "graph fixture units resolvable" False
        -- diamond: a <- b, a <- c, {b,c} <- d
        let a = mkU "pa" "lib" "a-1-inplace" []
            b = mkU "pb" "lib" "b-1-inplace" ["a-1-inplace"]
            c = mkU "pc" "lib" "c-1-inplace" ["a-1-inplace"]
            d = mkU "pd" "exe:d" "d-1-inplace-d" ["b-1-inplace", "c-1-inplace"]
            diamond = map puId (topoOrder [d, c, b, a])
        check "topoOrder deps-first (diamond)" (depsFirst (topoOrder [d, c, b, a]))
        checkEq "topoOrder diamond root first" (take 1 diamond) ["a-1-inplace"]
        checkEq "topoOrder diamond sink last" (take 1 (reverse diamond)) ["d-1-inplace-d"]

        putStrLn "== FFCabal.Graph.resolveTargets =="
        let rt ts = fmap (sort . map puId) (resolveTargets checkables ts)
        checkEq "full target" (rt ["ltk:lib:ltk"]) (Right ["ltk-0.1.0.0-inplace"])
        checkEq "bare exe target" (rt ["exe:leksah-wkwebview"])
            (Right ["leksah-0.17.0.0-inplace-leksah-wkwebview"])
        checkEq "bare sublib target" (rt ["lib:leksah-nogtk"])
            (Right ["leksah-0.17.0.0-inplace-leksah-nogtk"])
        checkEq "package name = all its components" (rt ["leksah"])
            (Right (sort [ "leksah-0.17.0.0-inplace-leksah-nogtk"
                         , "leksah-0.17.0.0-inplace-leksah-wkwebview" ]))
        checkEq "all" (rt ["all"]) (Right (sort (map puId checkables)))
        check "unknown target -> Left" $ case rt ["no-such-thing"] of
            Left e  -> "matches no local component" `T.isInfixOf` e
            Right _ -> False

    void . (try :: IO () -> IO (Either SomeException ())) $
        removeDirectoryRecursive planDir

    putStrLn "== integration (guarded) =="
    integrationTests check

    n <- readIORef failures
    if n == 0
      then putStrLn "ALL TESTS PASSED" >> exitSuccess
      else putStrLn (show n <> " FAILURE(S)") >> exitFailure
  where
    esc s = T.pack ('\ESC' : s)

-- deps-first: every unit appears after all its local deps
depsFirst :: [PlanUnit] -> Bool
depsFirst us = all ok (zip [(0 :: Int) ..] us)
  where
    idx = M.fromList (zip (map puId us) [0 ..])
    ok (i, u) = all (\d -> maybe True (< i) (M.lookup d idx))
                    (puDepends u ++ puExeDepends u)

mkU :: Text -> Text -> Text -> [Text] -> PlanUnit
mkU pkg comp uid deps = PlanUnit
  { puId = uid, puType = "configured", puStyle = Just "local"
  , puPkgName = pkg, puVersion = "0.1", puComponent = Just comp
  , puSrcPath = Just "/x", puDepends = deps, puExeDepends = [] }

-- A realistic plan.json fixture: pre-existing boot lib, a global configured
-- dep, local libs/sublib/exe/test, a setup unit, and a local unit with no
-- component-name.  Shapes copied from a real cabal 3.16 plan.
fixturePlanJson :: String
fixturePlanJson = unlines
  [ "{ \"cabal-version\": \"3.16.1.0\", \"compiler-id\": \"ghc-9.14.1\","
  , "  \"os\": \"osx\", \"arch\": \"aarch64\","
  , "  \"install-plan\": ["
  , "  { \"type\": \"pre-existing\", \"id\": \"base-4.22.0.0-inplace\","
  , "    \"pkg-name\": \"base\", \"pkg-version\": \"4.22.0.0\","
  , "    \"depends\": [] },"
  , "  { \"type\": \"configured\", \"id\": \"sn-2.2.5.0-61efbe57\", \"style\": \"global\","
  , "    \"pkg-name\": \"aeson\", \"pkg-version\": \"2.2.5.0\", \"component-name\": \"lib\","
  , "    \"depends\": [\"base-4.22.0.0-inplace\"] },"
  , "  { \"type\": \"configured\", \"id\": \"ltk-0.1.0.0-inplace\", \"style\": \"local\","
  , "    \"pkg-name\": \"ltk\", \"pkg-version\": \"0.1.0.0\", \"component-name\": \"lib\","
  , "    \"pkg-src\": { \"type\": \"local\", \"path\": \"/proj/vendor/ltk\" },"
  , "    \"depends\": [\"base-4.22.0.0-inplace\"] },"
  , "  { \"type\": \"configured\", \"id\": \"vcswrapper-0.1.0.0-inplace\", \"style\": \"local\","
  , "    \"pkg-name\": \"vcswrapper\", \"pkg-version\": \"0.1.0.0\", \"component-name\": \"lib\","
  , "    \"pkg-src\": { \"type\": \"local\", \"path\": \"/proj/vendor/vcs\" },"
  , "    \"depends\": [\"base-4.22.0.0-inplace\"] },"
  , "  { \"type\": \"configured\", \"id\": \"leksah-server-0.17.0.0-inplace\", \"style\": \"local\","
  , "    \"pkg-name\": \"leksah-server\", \"pkg-version\": \"0.17.0.0\", \"component-name\": \"lib\","
  , "    \"pkg-src\": { \"type\": \"local\", \"path\": \"/proj/vendor/leksah-server\" },"
  , "    \"depends\": [\"base-4.22.0.0-inplace\", \"sn-2.2.5.0-61efbe57\"] },"
  , "  { \"type\": \"configured\", \"id\": \"leksah-0.17.0.0-inplace-leksah-nogtk\", \"style\": \"local\","
  , "    \"pkg-name\": \"leksah\", \"pkg-version\": \"0.17.0.0\", \"component-name\": \"lib:leksah-nogtk\","
  , "    \"pkg-src\": { \"type\": \"local\", \"path\": \"/proj\" },"
  , "    \"depends\": [\"leksah-server-0.17.0.0-inplace\", \"ltk-0.1.0.0-inplace\","
  , "                  \"vcswrapper-0.1.0.0-inplace\", \"base-4.22.0.0-inplace\"] },"
  , "  { \"type\": \"configured\", \"id\": \"leksah-0.17.0.0-inplace-leksah-wkwebview\", \"style\": \"local\","
  , "    \"pkg-name\": \"leksah\", \"pkg-version\": \"0.17.0.0\", \"component-name\": \"exe:leksah-wkwebview\","
  , "    \"pkg-src\": { \"type\": \"local\", \"path\": \"/proj\" },"
  , "    \"bin-file\": \"/proj/dist/wk\","
  , "    \"depends\": [\"leksah-0.17.0.0-inplace-leksah-nogtk\", \"base-4.22.0.0-inplace\"] },"
  , "  { \"type\": \"configured\", \"id\": \"leksah-server-0.17.0.0-inplace-test-tool\", \"style\": \"local\","
  , "    \"pkg-name\": \"leksah-server\", \"pkg-version\": \"0.17.0.0\", \"component-name\": \"test:test-tool\","
  , "    \"pkg-src\": { \"type\": \"local\", \"path\": \"/proj/vendor/leksah-server\" },"
  , "    \"depends\": [\"leksah-server-0.17.0.0-inplace\", \"base-4.22.0.0-inplace\"] },"
  , "  { \"type\": \"configured\", \"id\": \"leksah-0.17.0.0-inplace-setup\", \"style\": \"local\","
  , "    \"pkg-name\": \"leksah\", \"pkg-version\": \"0.17.0.0\", \"component-name\": \"setup\","
  , "    \"pkg-src\": { \"type\": \"local\", \"path\": \"/proj\" },"
  , "    \"depends\": [\"base-4.22.0.0-inplace\"] },"
  , "  { \"type\": \"configured\", \"id\": \"odd-0.1-inplace\", \"style\": \"local\","
  , "    \"pkg-name\": \"odd\", \"pkg-version\": \"0.1\","
  , "    \"pkg-src\": { \"type\": \"local\", \"path\": \"/proj/odd\" },"
  , "    \"depends\": [] }"
  , "  ] }"
  ]

-- ---------------------------------------------------------------------------
-- Integration: drive the real binary against a generated fixture project on a
-- throwaway tmux server.

integrationTests :: (String -> Bool -> IO ()) -> IO ()
integrationTests check = do
    mghc   <- findExecutable "ghc"
    mcabal <- findExecutable "cabal"
    mtmux  <- findExecutable "tmux"
    mbin   <- resolveFfcabal
    case (mghc, mcabal, mtmux, mbin) of
      (Just _, Just _, Just _, Just bin) -> do
        pid <- getProcessID
        tmpRoot <- getTemporaryDirectory
        let sock = "ffcabal-test-" <> show pid
            proj = tmpRoot </> ("ffcabal-test-proj-" <> show pid)
        run bin sock proj `finally` cleanup sock proj
      _ -> putStrLn "SKIP integration (needs ghc, cabal, tmux on PATH and a built ffcabal — set FFCABAL_BIN or build first)"
  where
    cleanup sock proj = do
        void ((try (readProcessWithExitCode "tmux" ["-L", sock, "kill-server"] ""))
                :: IO (Either SomeException (ExitCode, String, String)))
        void ((try (removeDirectoryRecursive proj)) :: IO (Either SomeException ()))
        -- a killed server can leave its socket file behind; tidy it up
        uid <- getEffectiveUserID
        mtd <- lookupEnv "TMUX_TMPDIR"
        let dirs = maybe [] (: []) mtd ++ ["/tmp"]
        mapM_ (\d -> void ((try (removeFile (d </> ("tmux-" <> show uid) </> sock)))
                             :: IO (Either SomeException ()))) dirs

    run bin sock proj = do
        writeFixtureProject proj
        baseEnv <- getEnvironment
        let ffEnv = ("FFCABAL_TMUX_ARGS", "-L " <> sock <> " -f /dev/null")
                    : filter ((/= "FFCABAL_TMUX_ARGS") . fst) baseEnv
            ff args = readCreateProcessWithExitCode
                          ((proc bin args) { cwd = Just proj, env = Just ffEnv }) ""

        -- run 1: clean build
        (c1, o1, e1) <- ff ["build", "exe:exeb"]
        let t1 = o1 <> e1
        check "int: clean build exits 0" (c1 == ExitSuccess)
        check "int: liba checked" ("checking liba:lib:liba" `isInfixOf` t1)
        check "int: exeb checked" ("checking exeb:exe:exeb" `isInfixOf` t1)
        check "int: reports OK" ("ffcabal: OK" `isInfixOf` t1)
        (_, wins, _) <- readProcessWithExitCode "tmux"
            ["-L", sock, "list-windows", "-t", "ffcabal", "-F", "#{window_name}"] ""
        check "int: repl windows on scratch server" ("liba:lib:liba" `isInfixOf` wins)

        -- run 2: fail fast on an injected type error (cached repl :reload)
        let aFile = proj </> "liba" </> "src" </> "A.hs"
        good <- readFile aFile
        length good `seq` writeFile aFile (good <> "\nbad :: Int\nbad = \"not an int\"\n")
        (c2, o2, e2) <- ff ["build", "exe:exeb"]
        let t2 = o2 <> e2
        check "int: type error exits non-zero" (c2 /= ExitSuccess)
        check "int: liba reload used cached repl" (":reload in cached repl" `isInfixOf` t2)
        check "int: error text shown" ("bad = \"not an int\"" `isInfixOf` t2 || "Couldn't match" `isInfixOf` t2 || "No instance" `isInfixOf` t2)
        check "int: fail-fast: exeb never checked" (not ("checking exeb:exe:exeb" `isInfixOf` t2))

        -- run 3: fixed again -> cached repls, green
        writeFile aFile good
        (c3, o3, e3) <- ff ["build", "exe:exeb"]
        let t3 = o3 <> e3
        check "int: fixed build exits 0" (c3 == ExitSuccess)
        check "int: cached repl reused after fix" (":reload in cached repl" `isInfixOf` t3)

        -- run 4: version bump changes liba's UnitId -> repl respawned in place,
        -- and the OLD repl process must be KILLED (respawn-window -k), not
        -- leaked — a leak would accumulate a live ghci per config change.
        let panePids = do
                (_, out, _) <- readProcessWithExitCode "tmux"
                    ["-L", sock, "list-windows", "-t", "ffcabal", "-F", "#{window_name}\t#{pane_pid}"] ""
                return [ (n, pid) | l <- lines out
                       , let (n, r) = break (== '\t') l
                       , pid@(_ : _) <- [drop 1 r] ]
            libaWin = "liba:lib:liba"
            alive pid = do
                (c, _, _) <- readProcessWithExitCode "kill" ["-0", pid] ""
                return (c == ExitSuccess)
            waitDead n pid
              | n <= (0 :: Int) = return False
              | otherwise = alive pid >>= \case
                    False -> return True
                    True  -> threadDelay 100000 >> waitDead (n - 1) pid
        pidsBefore <- panePids
        let oldPid = lookup libaWin pidsBefore
        check "int: old repl pid captured" (oldPid /= Nothing)
        writeFile (proj </> "liba" </> "liba.cabal") (libaCabal "0.2.0.0")
        (c4, o4, e4) <- ff ["build", "exe:exeb"]
        let t4 = o4 <> e4
        check "int: unitid change exits 0" (c4 == ExitSuccess)
        check "int: unitid change respawns repl" ("repl restarted: configuration changed" `isInfixOf` t4)
        pidsAfter <- panePids
        check "int: liba window not duplicated"
            (length (filter ((== libaWin) . fst) pidsAfter) == 1)
        check "int: respawned repl is a new process"
            (lookup libaWin pidsAfter /= Nothing && lookup libaWin pidsAfter /= oldPid)
        oldDead <- maybe (return False) (waitDead 50) oldPid
        check "int: old repl process killed" oldDead
        -- nothing left in the old process group either (children like ghc)
        groupGone <- case oldPid of
            Nothing -> return False
            Just p  -> do
                (c, _, _) <- readProcessWithExitCode "pgrep" ["-g", p] ""
                return (c /= ExitSuccess)   -- no members found
        check "int: old repl process group empty" groupGone

        -- run 5: a cabal-file edit that does NOT change the UnitId (inplace
        -- ids are stable across cabal edits) must still respawn the repl —
        -- a cached ghci never re-reads module lists (the confhash check).
        appendFile (proj </> "liba" </> "liba.cabal") "\n-- confhash test\n"
        (c5, o5, e5) <- ff ["build", "exe:exeb"]
        let t5 = o5 <> e5
        check "int: cabal-file edit exits 0" (c5 == ExitSuccess)
        check "int: cabal-file edit respawns repl (confhash)"
            ("repl restarted: configuration changed" `isInfixOf` t5)
        -- run 6: unchanged again -> cached repls
        (c6, o6, e6) <- ff ["build", "exe:exeb"]
        let t6 = o6 <> e6
        check "int: post-edit build exits 0" (c6 == ExitSuccess)
        check "int: repl cached again after confhash settles"
            (":reload in cached repl" `isInfixOf` t6)

    resolveFfcabal = lookupEnv "FFCABAL_BIN" >>= \case
        Just p | not (null p) -> do
            ok <- doesFileExist p
            return (if ok then Just p else Nothing)
        _ -> (try (readProcessWithExitCode "cabal" ["list-bin", "ffcabal"] "")
                :: IO (Either SomeException (ExitCode, String, String))) >>= \case
            Right (ExitSuccess, out, _) ->
                case lines out of
                  (p : _) | not (null p) -> do
                      ok <- doesFileExist p
                      return (if ok then Just p else Nothing)
                  _ -> return Nothing
            _ -> return Nothing

libaCabal :: String -> String
libaCabal ver = unlines
  [ "cabal-version: 3.0"
  , "name: liba"
  , "version: " <> ver
  , "build-type: Simple"
  , "library"
  , "  hs-source-dirs: src"
  , "  exposed-modules: A"
  , "  build-depends: base"
  , "  default-language: Haskell2010"
  ]

writeFixtureProject :: FilePath -> IO ()
writeFixtureProject proj = do
    createDirectoryIfMissing True (proj </> "liba" </> "src")
    createDirectoryIfMissing True (proj </> "exeb")
    writeFile (proj </> "cabal.project") "packages: liba/ exeb/\n"
    writeFile (proj </> "liba" </> "liba.cabal") (libaCabal "0.1.0.0")
    writeFile (proj </> "liba" </> "src" </> "A.hs") $ unlines
        [ "module A (aValue) where"
        , "aValue :: Int"
        , "aValue = 42"
        ]
    writeFile (proj </> "exeb" </> "exeb.cabal") $ unlines
        [ "cabal-version: 3.0"
        , "name: exeb"
        , "version: 0.1.0.0"
        , "build-type: Simple"
        , "executable exeb"
        , "  main-is: Main.hs"
        , "  build-depends: base, liba"
        , "  default-language: Haskell2010"
        ]
    writeFile (proj </> "exeb" </> "Main.hs") $ unlines
        [ "module Main (main) where"
        , "import A (aValue)"
        , "main :: IO ()"
        , "main = print aValue"
        ]

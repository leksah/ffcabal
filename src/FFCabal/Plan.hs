{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | Reading cabal's @plan.json@ (written by @cabal build --dry-run@ into
-- @\<builddir\>\/cache\/plan.json@).  Only the fields ffcabal needs: unit
-- identity, locality, component naming and the dependency edges.  UnitIds are
-- treated as opaque strings — matched by equality against @depends@, never
-- parsed (this repo's cabal abbreviates package names inside hashes).
module FFCabal.Plan
  ( PlanUnit(..)
  , readPlan
  , isLocalUnit
  , isCheckableComponent
  , unitTarget
  , configHash
  ) where

import Data.Aeson
       (FromJSON(..), eitherDecodeFileStrict', withObject, (.:), (.:?),
        (.!=), Value(Object))
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8 (pack)
import Data.List (foldl', nub, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (forM)
import Data.Word (Word64)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), isAbsolute, takeExtension)
import Text.Printf (printf)

data PlanUnit = PlanUnit
  { puId         :: Text            -- ^ UnitId, e.g. @ltk-0.16.2.0-inplace@
  , puType       :: Text            -- ^ @configured@ | @pre-existing@
  , puStyle      :: Maybe Text      -- ^ @local@ for project packages
  , puPkgName    :: Text
  , puVersion    :: Text
  , puComponent  :: Maybe Text      -- ^ @lib@ | @lib:x@ | @exe:x@ | @test:x@ | @bench:x@ | @setup@
  , puSrcPath    :: Maybe FilePath  -- ^ pkg-src.path for local packages
  , puDepends    :: [Text]          -- ^ UnitIds
  , puExeDepends :: [Text]          -- ^ build-tool UnitIds
  } deriving (Show, Eq)

instance FromJSON PlanUnit where
  parseJSON = withObject "PlanUnit" $ \o -> PlanUnit
    <$> o .: "id"
    <*> o .: "type"
    <*> o .:? "style"
    <*> o .: "pkg-name"
    <*> o .: "pkg-version"
    <*> o .:? "component-name"
    <*> (o .:? "pkg-src" >>= \case
           Just (Object src) -> src .:? "path"
           _                 -> pure Nothing)
    <*> o .:? "depends" .!= []
    <*> o .:? "exe-depends" .!= []

newtype Plan = Plan [PlanUnit]

instance FromJSON Plan where
  parseJSON = withObject "Plan" $ \o -> Plan <$> o .: "install-plan"

-- | Read @\<builddir\>\/cache\/plan.json@.
readPlan :: FilePath -> IO (Either String [PlanUnit])
readPlan builddir =
    fmap (\(Plan us) -> us) <$> eitherDecodeFileStrict' (builddir </> "cache" </> "plan.json")

-- | A component of one of the project's own packages.
isLocalUnit :: PlanUnit -> Bool
isLocalUnit u = puType u == "configured" && puStyle u == Just "local"

-- | Something @cabal repl@ can load (libs and exes; not @setup@).
isCheckableComponent :: PlanUnit -> Bool
isCheckableComponent u = case puComponent u of
    Just c  -> c /= "setup"
    Nothing -> False

-- | The cabal target string for a unit, e.g. @ltk:lib:ltk@,
-- @leksah:lib:leksah-nogtk@, @leksah:exe:leksah-cmd@.
unitTarget :: PlanUnit -> Text
unitTarget u = case fromMaybe "" (puComponent u) of
    "lib" -> p <> ":lib:" <> p
    c | T.null c  -> p
      | otherwise -> p <> ":" <> c
  where p = puPkgName u

-- | A hash over everything that can change a component's ghci session
-- WITHOUT changing its UnitId: every local package's .cabal file plus the
-- cabal.project(.local/.freeze) files.  Inplace UnitIds are stable across
-- cabal-file edits (adding a module, changing exposed-modules), but a cached
-- ghci never re-reads them — cross-package imports then fail against the
-- stale module lists.  The hash is recorded on each repl window
-- (@\@ffcabal_confhash@); a mismatch respawns the repl.
configHash :: FilePath -> [PlanUnit] -> IO String
configHash root units = do
    cabalFiles <- fmap concat . forM (nub (mapMaybe puSrcPath units)) $ \d -> do
        let dir = if isAbsolute d then d else root </> d
        ok <- doesDirectoryExist dir
        if ok
          then map (dir </>) . filter ((== ".cabal") . takeExtension)
                 <$> listDirectory dir
          else return []
    let projFiles = map (root </>)
            ["cabal.project", "cabal.project.local", "cabal.project.freeze"]
    chunks <- forM (sort (nub (projFiles ++ cabalFiles))) $ \f ->
        doesFileExist f >>= \case
            False -> return mempty
            True  -> (BS8.pack f <>) <$> BS.readFile f
    return (printf "%016x" (fnv1a (BS.concat chunks)))

-- | FNV-1a (64-bit) — no crypto needed, just cheap change detection.
fnv1a :: BS.ByteString -> Word64
fnv1a = BS.foldl' step 0xcbf29ce484222325
  where step h b = (h `xor` fromIntegral b) * 0x100000001b3

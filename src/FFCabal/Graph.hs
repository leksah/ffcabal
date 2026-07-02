{-# LANGUAGE OverloadedStrings #-}
-- | The local-unit dependency graph: reachability from requested targets and
-- a dependencies-first topological order (like Leksah's @IDE.Build@, but at
-- unit granularity straight from plan.json).
module FFCabal.Graph
  ( localDeps
  , reachable
  , topoOrder
  , resolveTargets
  ) where

import Data.Graph (graphFromEdges, topSort)
import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import FFCabal.Plan (PlanUnit(..), unitTarget)

-- | A unit's dependencies restricted to the given (local) unit set.
localDeps :: M.Map Text PlanUnit -> PlanUnit -> [PlanUnit]
localDeps byId u =
    [ d | i <- nub (puDepends u ++ puExeDepends u), Just d <- [M.lookup i byId] ]

-- | All units reachable (via local deps) from the given starting units.
reachable :: [PlanUnit] -> [PlanUnit] -> [PlanUnit]
reachable units starts = [ u | u <- units, puId u `S.member` closure ]
  where
    byId = M.fromList [ (puId u, u) | u <- units ]
    closure = go S.empty (map puId starts)
    go seen [] = seen
    go seen (i:rest)
      | i `S.member` seen = go seen rest
      | otherwise = case M.lookup i byId of
          Nothing -> go seen rest
          Just u  -> go (S.insert i seen) (puDepends u ++ puExeDepends u ++ rest)

-- | Dependencies-first order of the given units.
topoOrder :: [PlanUnit] -> [PlanUnit]
topoOrder units = reverse [ u | v <- topSort g, let (u, _, _) = fromVertex v ]
  where
    ids = S.fromList (map puId units)
    (g, fromVertex, _) = graphFromEdges
      [ (u, puId u, filter (`S.member` ids) (nub (puDepends u ++ puExeDepends u)))
      | u <- units ]

-- | Resolve user target strings against the local units.  Accepts full
-- @pkg:comp:name@ targets, bare component targets (@exe:foo@, @lib:foo@,
-- @test:foo@), and package names (all of that package's components).
-- Returns @Left unknown@ if a target matches nothing.
resolveTargets :: [PlanUnit] -> [Text] -> Either Text [PlanUnit]
resolveTargets units = fmap (nub . concat) . traverse resolve1
  where
    resolve1 t =
      let matches = filter (matchesTarget t) units
      in if null matches
           then Left $ "target '" <> t <> "' matches no local component; known: "
                       <> T.intercalate ", " (map unitTarget units)
           else Right matches
    matchesTarget t u =
         t == unitTarget u
      || Just t == puComponent u                    -- "exe:foo", "lib:foo", "lib"
      || t == puPkgName u                           -- whole package
      || t == "all"

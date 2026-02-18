{-# LANGUAGE ScopedTypeVariables #-}

-- | Presheaf operations for the hidden variable debates analysis.
-- Implements factorizability checks, global section detection,
-- and the valuation presheaf for the Kochen-Specker theorem.
module Presheaf where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub, subsequences, permutations)

-- | A context: a set of mutually commuting observables.
type Context = Set String

-- | A measurement scenario: observables, contexts, and outcomes.
data MeasurementScenario = MeasurementScenario
  { msObservables :: Set String
  , msContexts    :: [Context]
  , msOutcomes    :: Set Int
  } deriving (Show)

-- | A local section: value assignment within a single context.
type LocalSection = Map String Int

-- | A global section: value assignment to all observables.
type GlobalSection = Map String Int

-- | The valuation presheaf V(C) = {value assignments on context C}.
valuationPresheaf :: MeasurementScenario -> Context -> [LocalSection]
valuationPresheaf scenario ctx =
  let obs = Set.toList ctx
      outs = Set.toList (msOutcomes scenario)
      -- Generate all possible assignments
      assignments = sequence [[(o, v) | v <- outs] | o <- obs]
  in map Map.fromList assignments

-- | Check if a local section is consistent with another on their overlap.
sectionsCompatible :: LocalSection -> LocalSection -> Bool
sectionsCompatible s1 s2 =
  let commonKeys = Set.intersection (Map.keysSet s1) (Map.keysSet s2)
  in all (\k -> Map.lookup k s1 == Map.lookup k s2) (Set.toList commonKeys)

-- | Check if a global section exists: can all local sections be glued?
hasGlobalSection :: MeasurementScenario -> [LocalSection] -> Bool
hasGlobalSection scenario localSections =
  let allObs = Set.toList (msObservables scenario)
      outs = Set.toList (msOutcomes scenario)
      -- Try all possible global assignments
      globalAssignments = sequence [[(o, v) | v <- outs] | o <- allObs]
      globals = map Map.fromList globalAssignments
  in any (\g -> all (\ls -> sectionsCompatible g ls) localSections) globals

-- | An empirical model: probability distributions for each context.
type EmpiricalModel = Map Context (Map LocalSection Double)

-- | Floating-point comparison tolerance for probability checks.
epsilon :: Double
epsilon = 1e-10

-- | Check if an empirical model admits a non-contextual explanation.
-- A non-contextual model = global probability distribution whose
-- marginals match the empirical model.
-- Note: all probability comparisons use epsilon tolerance to avoid
-- floating-point issues in exact categorical obstruction checks.
isNonContextual :: MeasurementScenario -> EmpiricalModel -> Bool
isNonContextual scenario model =
  -- Simplified check: just verify global section existence for support
  let supportSections = concatMap (\(_, dist) ->
        [s | (s, p) <- Map.toList dist, p > epsilon]
        ) (Map.toList model)
  in hasGlobalSection scenario supportSections

-- | Contextual fraction: maximum weight of non-contextual sub-model.
-- Returns 0 for fully contextual, 1 for non-contextual.
-- (Simplified version using support analysis.)
contextualFraction :: MeasurementScenario -> EmpiricalModel -> Double
contextualFraction scenario model =
  if isNonContextual scenario model then 1.0 else 0.0

-- | Build the CHSH measurement scenario.
chshScenario :: MeasurementScenario
chshScenario = MeasurementScenario
  { msObservables = Set.fromList ["A1", "A2", "B1", "B2"]
  , msContexts    = [ Set.fromList ["A1", "B1"]
                    , Set.fromList ["A1", "B2"]
                    , Set.fromList ["A2", "B1"]
                    , Set.fromList ["A2", "B2"]
                    ]
  , msOutcomes    = Set.fromList [-1, 1]
  }

-- | Build the Peres-Mermin magic square scenario.
peresMerminScenario :: MeasurementScenario
peresMerminScenario =
  let obs = Set.fromList
        [ "X1", "1X", "XX"    -- Row 1
        , "1Y", "Y1", "YY"    -- Row 2
        , "XY", "YX", "ZZ"    -- Row 3
        ]
      rows = [ Set.fromList ["X1", "1X", "XX"]
             , Set.fromList ["1Y", "Y1", "YY"]
             , Set.fromList ["XY", "YX", "ZZ"]
             ]
      cols = [ Set.fromList ["X1", "1Y", "XY"]
             , Set.fromList ["1X", "Y1", "YX"]
             , Set.fromList ["XX", "YY", "ZZ"]
             ]
  in MeasurementScenario
    { msObservables = obs
    , msContexts    = rows ++ cols
    , msOutcomes    = Set.fromList [-1, 1]
    }

-- | Verify the Kochen-Specker obstruction for the Peres-Mermin square.
-- Returns True if NO non-contextual assignment exists (KS obstruction present).
verifyKochenSpeckerPeresMermin :: Bool
verifyKochenSpeckerPeresMermin =
  let scenario = peresMerminScenario
      obs = Set.toList (msObservables scenario)
      -- Generate all possible {-1, +1} assignments to 9 observables
      allAssignments = sequence [[(o, v) | v <- [-1, 1]] | o <- obs]
      globals = map Map.fromList allAssignments
      -- Check product constraints:
      -- Row products = +1: X1*1X*XX = 1, 1Y*Y1*YY = 1, XY*YX*ZZ = 1
      -- Column products = +1 for cols 1,2; = -1 for col 3
      checkConstraints g =
        let lk k = case Map.lookup k g of
              Just v -> v
              Nothing -> error $ "Missing key: " ++ k
            row1 = lk "X1" * lk "1X" * lk "XX"
            row2 = lk "1Y" * lk "Y1" * lk "YY"
            row3 = lk "XY" * lk "YX" * lk "ZZ"
            col1 = lk "X1" * lk "1Y" * lk "XY"
            col2 = lk "1X" * lk "Y1" * lk "YX"
            col3 = lk "XX" * lk "YY" * lk "ZZ"
        in row1 == 1 && row2 == 1 && row3 == 1
           && col1 == 1 && col2 == 1 && col3 == (-1)
  in not (any checkConstraints globals)

{-# LANGUAGE ScopedTypeVariables #-}

-- | Verification of the Kochen-Specker theorem as a presheaf obstruction.
-- Implements the Peres-Mermin magic square proof and connects it
-- to the Yoneda Constraint framework.
module KochenSpecker where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Presheaf (verifyKochenSpeckerPeresMermin, peresMerminScenario,
                 MeasurementScenario(..), valuationPresheaf)
import qualified Data.Set as Set

-- | The Peres-Mermin magic square operators.
-- Each entry is a tensor product of Pauli matrices.
--
--   sigma_x (x) I     |  I (x) sigma_x    |  sigma_x (x) sigma_x
--   I (x) sigma_y     |  sigma_y (x) I     |  sigma_y (x) sigma_y
--   sigma_x (x) sigma_y | sigma_y (x) sigma_x | sigma_z (x) sigma_z
--
-- Row products = +I (identity)
-- Column products = +I, +I, -I
data MagicSquare = MagicSquare
  { msEntries :: [[String]]
  , msRowProducts :: [Int]     -- Expected product for each row
  , msColProducts :: [Int]     -- Expected product for each column
  } deriving (Show)

-- | The standard Peres-Mermin magic square.
standardMagicSquare :: MagicSquare
standardMagicSquare = MagicSquare
  { msEntries = [ ["X1", "1X", "XX"]
                , ["1Y", "Y1", "YY"]
                , ["XY", "YX", "ZZ"]
                ]
  , msRowProducts = [1, 1, 1]
  , msColProducts = [1, 1, -1]
  }

-- | Verify the algebraic contradiction in the Peres-Mermin square.
-- The product of all row products should equal the product of all column products,
-- but rows give (+1)^3 = +1 while columns give (+1)^2 * (-1) = -1.
algebraicContradiction :: MagicSquare -> Bool
algebraicContradiction ms =
  let rowProd = product (msRowProducts ms)
      colProd = product (msColProducts ms)
  in rowProd /= colProd

-- | Count the number of valid local sections for each context.
localSectionCounts :: MeasurementScenario -> Map (Set.Set String) Int
localSectionCounts scenario =
  Map.fromList
    [ (ctx, length (valuationPresheaf scenario ctx))
    | ctx <- msContexts scenario
    ]

-- | Full KS verification result.
data KSResult = KSResult
  { ksAlgebraicContradiction :: Bool
  , ksNoGlobalSection        :: Bool
  , ksLocalSections          :: Map (Set.Set String) Int
  , ksInterpretation         :: String
  } deriving (Show)

-- | Run the complete Kochen-Specker verification.
runKSVerification :: KSResult
runKSVerification =
  let ms = standardMagicSquare
      scenario = peresMerminScenario
  in KSResult
    { ksAlgebraicContradiction = algebraicContradiction ms
    , ksNoGlobalSection = verifyKochenSpeckerPeresMermin
    , ksLocalSections = localSectionCounts scenario
    , ksInterpretation = unlines
        [ "The Peres-Mermin magic square demonstrates the Kochen-Specker theorem:"
        , "  1. Each row/column defines a context of commuting observables."
        , "  2. Local sections (value assignments) exist for each context."
        , "  3. No global section exists: local sections cannot be glued."
        , "  4. This is a presheaf obstruction: the valuation presheaf"
        , "     V: C_Q^op -> Set has no global section."
        , "  5. From the Yoneda perspective: no object in Meas_Q has a"
        , "     representable presheaf yielding a non-contextual valuation."
        , ""
        , "The algebraic proof:"
        , "  - Row products: (+1) * (+1) * (+1) = +1"
        , "  - Column products: (+1) * (+1) * (-1) = -1"
        , "  - Each value v(O) in {-1, +1} is used exactly once per row/column"
        , "  - Product of all entries by rows = +1"
        , "  - Product of all entries by columns = -1"
        , "  - Contradiction: same values, different products."
        ]
    }

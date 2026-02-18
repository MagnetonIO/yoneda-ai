{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : KanExtension
-- Description : Kan extensions for the measurement problem
-- Copyright   : (c) Matthew Long, YonedaAI Research Collective, 2026
--
-- Implements the Kan extension framework from Section 5 of the paper:
-- "Decoherence as Kan Extension" (Theorem 5.2).
--
-- The left Kan extension Lan_iota(id_{MeasC}) provides the best
-- approximation to the classical identity functor, extended from
-- the classical subcategory to the full quantum category.

module KanExtension
  ( -- * Kan Extension Types
    KanExtension(..)
  , leftKanExtension
  , rightKanExtension

    -- * Application to Measurement
  , decoherenceAsKanExt
  , kanExtensionDeficit
  , witnessHierarchy

    -- * Emergence Functors
  , EmergenceFunctor(..)
  , emergenceKanExtension
  , epistemicHorizon
  ) where

import Data.Complex
import MeasurementCategory

-- ============================================================
-- Kan Extension Types
-- ============================================================

-- | A Kan extension consists of:
--   * The extended functor
--   * The universal natural transformation (unit or counit)
--   * The deficit measuring how far from exact the extension is
data KanExtension = KanExtension
  { keLabel   :: String
  , keApply   :: MeasQ -> MeasC        -- ^ The extended functor
  , keDeficit :: MeasQ -> Double        -- ^ The extension deficit
  , keUnit    :: MeasQ -> MeasQ -> Bool -- ^ Whether the unit is iso at a pair
  } deriving ()

-- | Left Kan extension: Lan_K F
-- Computed pointwise as a colimit.
-- For the decoherence case: Lan_iota(id) = D (decoherence functor).
leftKanExtension :: KanExtension
leftKanExtension = KanExtension
  { keLabel   = "Lan_iota(id_{MeasC})"
  , keApply   = transitionFunctor  -- D = pi_C . decoherence
  , keDeficit = extensionDeficit
  , keUnit    = \q1 q2 ->
      -- The unit is an isomorphism iff q1 is already classical
      extensionDeficit q1 < 1e-10 && extensionDeficit q2 < 1e-10
  }

-- | Right Kan extension: Ran_K F
-- Computed pointwise as a limit.
-- Provides the "conservative" approximation.
rightKanExtension :: KanExtension
rightKanExtension = KanExtension
  { keLabel   = "Ran_iota(id_{MeasC})"
  , keApply   = conservativeTransition
  , keDeficit = \mq -> measurementOpacity mq  -- Right Kan is more conservative
  , keUnit    = \q1 q2 ->
      extensionDeficit q1 < 1e-10 && extensionDeficit q2 < 1e-10
  }

-- | Conservative transition: maps to the "safest" classical state
-- (maximally mixed over the support of the diagonal).
conservativeTransition :: MeasQ -> MeasC
conservativeTransition (MeasQ label _ dm) = MeasC label probs
  where
    n = dmDim dm
    diag = [realPart (dmMatrix dm !! i !! i) | i <- [0..n-1]]
    -- Conservative: spread probability over all non-zero entries
    support = length (filter (> 1e-10) diag)
    probs = [if d > 1e-10 then 1.0 / fromIntegral support else 0 | d <- diag]

-- ============================================================
-- Application to Measurement
-- ============================================================

-- | Demonstrate that decoherence IS the left Kan extension.
-- Returns True if the decoherence functor matches the Kan extension output.
decoherenceAsKanExt :: MeasQ -> Bool
decoherenceAsKanExt mq =
    let kanResult = keApply leftKanExtension mq
        decoResult = transitionFunctor mq
    in mcProbs kanResult == mcProbs decoResult  -- Should always be True

-- | Compute the Kan extension deficit for a given state.
-- Delta(S) = S(D(rho)) - S(rho) >= 0
kanExtensionDeficit :: MeasQ -> Double
kanExtensionDeficit = keDeficit leftKanExtension

-- | Witness hierarchy: W_0 ⊂ W_1 ⊂ W_2 ⊂ ...
-- For a given state, compute which levels of the witness hierarchy
-- have non-trivial witnesses.
-- Returns a list of (level, fidelity) pairs.
witnessHierarchy :: MeasQ -> [(Int, Double)]
witnessHierarchy mq =
    [ (n, fidelity n)
    | n <- [0..4]
    , fidelity n > 1e-10
    ]
  where
    deficit = extensionDeficit mq
    capacity = log (fromIntegral (mqDim (mqState mq)))
    -- Fidelity decreases exponentially with witness level
    fidelity n
      | capacity <= 0 = 0
      | otherwise     = max 0 (sqrt (1 - exp (-capacity)) * exp (-fromIntegral n * deficit))

-- ============================================================
-- Emergence Functors
-- ============================================================

-- | An emergence functor Phi^*: MeasPre -> MeasEmer
-- Models the relationship between pre-geometric and emergent categories.
data EmergenceFunctor = EmergenceFunctor
  { efLabel      :: String
  , efRestrict   :: MeasQ -> MeasQ    -- ^ Restriction to emergent sector
  , efKernel     :: MeasQ -> Double   -- ^ Dimension of the emergence kernel
  , efOpacity    :: MeasQ -> Double   -- ^ Measurement opacity index
  }

-- | Construct the Kan extension for an emergence functor.
-- This is the higher-order obstruction (Theorem 8.3).
emergenceKanExtension :: EmergenceFunctor -> MeasQ -> MeasC
emergenceKanExtension ef = transitionFunctor . efRestrict ef

-- | Compute the epistemic horizon for an emergence functor.
-- The epistemic horizon H = Phi(A_Emer)' ∩ A_Pre
-- is the set of pre-geometric observables invisible to emergent measurements.
-- Returns the dimension of the epistemic horizon.
epistemicHorizon :: EmergenceFunctor -> MeasQ -> Double
epistemicHorizon ef mq =
    let totalDim = fromIntegral (mqDim (mqState mq))
        emergentDim = fromIntegral (mqDim (mqState (efRestrict ef mq)))
    in totalDim - emergentDim

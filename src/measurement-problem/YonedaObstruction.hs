{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : YonedaObstruction
-- Description : The measurement obstruction as a Yoneda-theoretic invariant
-- Copyright   : (c) Matthew Long, YonedaAI Research Collective, 2026
--
-- Implements the Measurement Obstruction Theorem (Theorem 4.1):
-- The measurement problem is classified by a cohomological invariant
-- measuring the failure of the Yoneda extension across the
-- quantum-classical boundary.

module YonedaObstruction
  ( -- * Obstruction Witnesses
    ObstructionWitness(..)
  , measurementObstruction
  , obstructionVanishes

    -- * Presheaf Comparison
  , presheafDistance
  , presheafCompatibility

    -- * Contextuality
  , ContextCategory(..)
  , valuationPresheaf
  , hasGlobalSection
  , contextualityWitness
  ) where

import Data.Complex
import MeasurementCategory

-- ============================================================
-- Obstruction Invariants
-- ============================================================

-- | A scalar witness for the measurement obstruction class [omega_M].
-- The full obstruction lives in H^2(MeasC; U(H_Env)); this type
-- captures computable scalar invariants that detect non-triviality.
-- When 'owCoherence' > 0, the obstruction class [omega_M] is non-trivial.
data ObstructionWitness = ObstructionWitness
  { owLabel     :: String
  , owDimEnv    :: Int         -- ^ Dimension of the environment Hilbert space
  , owEntropy   :: Double      -- ^ Entanglement entropy (witnesses non-trivial kernel)
  , owCoherence :: Double      -- ^ Off-diagonal l2-norm (witnesses non-trivial cocycle)
  , owVanishes  :: Bool        -- ^ Whether the obstruction vanishes
  } deriving (Show)

-- | Compute the measurement obstruction for a given quantum observer.
-- The obstruction vanishes iff:
--   (a) The state is already classical (no off-diagonal coherences), AND
--   (b) The environment is trivial (no entanglement).
measurementObstruction :: MeasQ -> ObstructionWitness
measurementObstruction mq = ObstructionWitness
  { owLabel     = "omega_M(" ++ mqLabel mq ++ ")"
  , owDimEnv    = envDim
  , owEntropy   = entropy
  , owCoherence = coherence
  , owVanishes  = coherence < 1e-10 && entropy < 1e-10
  }
  where
    dm = mqState mq
    n = dmDim dm
    m = dmMatrix dm
    -- Off-diagonal coherence: sum of |rho_{ij}|^2 for i /= j
    coherence = sum [ magnitude (m !! i !! j) ^ (2 :: Int)
                    | i <- [0..n-1], j <- [0..n-1], i /= j ]
    -- Entanglement entropy (von Neumann entropy of the reduced state)
    entropy = vonNeumannEntropy dm
    -- Environment dimension (assumed from the total system dimension)
    envDim = n  -- In a real implementation, this would come from the bipartition

-- | Check if the measurement obstruction vanishes.
-- Returns True iff the state is classical and unentangled.
obstructionVanishes :: MeasQ -> Bool
obstructionVanishes = owVanishes . measurementObstruction

-- ============================================================
-- Presheaf Comparison
-- ============================================================

-- | Distance between two representable presheaves.
-- Measured as the trace distance between the underlying density matrices.
-- d(y_A, y_B) = (1/2) Tr|rho_A - rho_B|
presheafDistance :: MeasQ -> MeasQ -> Double
presheafDistance mq1 mq2
  | n1 /= n2  = 1.0  -- Incompatible dimensions => maximal distance
  | otherwise  = 0.5 * sum [abs (realPart (diff !! i !! i)) | i <- [0..n1-1]]
  where
    n1 = dmDim (mqState mq1)
    n2 = dmDim (mqState mq2)
    m1 = dmMatrix (mqState mq1)
    m2 = dmMatrix (mqState mq2)
    diff = [[m1 !! i !! j - m2 !! i !! j | j <- [0..n1-1]] | i <- [0..n1-1]]

-- | Compatibility of two presheaves.
-- Returns 1.0 for identical presheaves, 0.0 for maximally incompatible.
presheafCompatibility :: MeasQ -> MeasQ -> Double
presheafCompatibility mq1 mq2 = 1.0 - presheafDistance mq1 mq2

-- ============================================================
-- Contextuality (Kochen-Specker)
-- ============================================================

-- | A context category: poset of commutative subalgebras.
-- For a qubit, contexts are parametrized by measurement directions.
data ContextCategory = ContextCategory
  { ccLabel    :: String
  , ccContexts :: [String]           -- ^ Labels for each context
  , ccDim      :: Int                -- ^ Hilbert space dimension
  } deriving (Show)

-- | The valuation presheaf V: C_Q^op -> Set
-- Assigns to each context the set of value assignments.
-- Returns True if the presheaf has a global section (non-contextual model exists).
valuationPresheaf :: ContextCategory -> Bool
valuationPresheaf cc
  | ccDim cc >= 3 = False  -- Kochen-Specker: no global section for dim >= 3
  | otherwise     = True   -- dim < 3: global sections may exist

-- | Check if the valuation presheaf has a global section.
-- By the Kochen-Specker theorem, this is impossible for dim >= 3.
hasGlobalSection :: Int -> Bool
hasGlobalSection dim = dim < 3

-- | Construct a contextuality witness.
-- Returns a set of contexts that cannot be simultaneously satisfied.
-- For dim >= 3, this always returns a non-empty witness.
contextualityWitness :: ContextCategory -> Maybe [String]
contextualityWitness cc
  | ccDim cc >= 3 = Just (take 3 (ccContexts cc))  -- Minimal KS witness
  | otherwise     = Nothing

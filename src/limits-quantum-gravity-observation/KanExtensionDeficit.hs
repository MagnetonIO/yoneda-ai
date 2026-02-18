{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Kan Extension Deficit Computations for Quantum Gravity
--
-- Implements the Kan extension machinery from the Yoneda Constraint
-- framework, specialized to the quantum gravitational measurement
-- category. Provides explicit computations of extension deficits
-- for various physical scenarios.
--
-- Reference: Long (2026), "Limits of Quantum Gravity Observation"

module KanExtensionDeficit
  ( -- * Kan extension types
    KanExtension(..)
  , computeLeftKan
  , computeRightKan
  , extensionBracket
    -- * Deficit decomposition
  , DeficitDecomposition(..)
  , decomposeDeficit
    -- * Convergence under expanding observation
  , convergenceSequence
  , isConverged
    -- * Specific models
  , jcModelDeficit      -- Jaynes-Cummings
  , spinChainDeficit    -- Spin chain
  , adsCftDeficit       -- AdS/CFT
  ) where

import QGMeasurementCategory

-- ============================================================
-- Kan Extension Types
-- ============================================================

-- | A Kan extension with its approximation quality
data KanExtension = KanExtension
  { kanType        :: KanType        -- ^ Left or Right
  , kanSource      :: QGObject       -- ^ Observer (source of extension)
  , kanApproxRank  :: Double         -- ^ Rank of the approximation
  , kanDeficitRank :: Double         -- ^ Rank of the deficit (cokernel)
  , kanComponents  :: Int            -- ^ Number of non-trivial components
  } deriving (Show)

data KanType = LeftKan | RightKan deriving (Show, Eq)

-- | Compute the left Kan extension (optimistic: colimit-based)
computeLeftKan :: QGObject -> Double -> KanExtension
computeLeftKan observer totalInfo =
  let -- Observer information content
      obsInfo = observerInformation observer
      -- Left Kan gives the best colimit approximation
      approxRank = min obsInfo totalInfo
      deficitRank = totalInfo - approxRank
      components = floor approxRank
  in KanExtension
       { kanType        = LeftKan
       , kanSource      = observer
       , kanApproxRank  = approxRank
       , kanDeficitRank = deficitRank
       , kanComponents  = components
       }

-- | Compute the right Kan extension (conservative: limit-based)
computeRightKan :: QGObject -> Double -> KanExtension
computeRightKan observer totalInfo =
  let -- Right Kan gives the best limit approximation
      obsInfo = observerInformation observer
      -- More conservative: assumes minimal correlations
      approxRank = min obsInfo totalInfo * 0.7  -- Heuristic factor
      deficitRank = totalInfo - approxRank
      components = floor approxRank
  in KanExtension
       { kanType        = RightKan
       , kanSource      = observer
       , kanApproxRank  = approxRank
       , kanDeficitRank = deficitRank
       , kanComponents  = components
       }

-- | The extension bracket: [Lan, Ran] measures fundamental ambiguity
extensionBracket :: QGObject -> Double -> (Double, Double)
extensionBracket observer totalInfo =
  let leftKan  = computeLeftKan observer totalInfo
      rightKan = computeRightKan observer totalInfo
  in (kanDeficitRank rightKan, kanDeficitRank leftKan)

-- | Compute observer information content
observerInformation :: QGObject -> Double
observerInformation obj =
  let entropy = matterEntropy (qgMatter obj)
      area    = regionArea (qgRegion obj)
      holoBound = bekensteinHawkingEntropy area
  in min entropy holoBound

-- ============================================================
-- Deficit Decomposition
-- ============================================================

-- | Full decomposition of the extension deficit
data DeficitDecomposition = DeficitDecomposition
  { ddQuantum       :: Double    -- ^ Entanglement entropy contribution
  , ddGravitational :: Double    -- ^ Black hole horizon contribution
  , ddGauge         :: Double    -- ^ Diffeomorphism gauge contribution
  , ddHolographic   :: Double    -- ^ Bekenstein-Hawking bound
  , ddInteraction   :: Double    -- ^ Cross-term from gravity-quantum interaction
  , ddTotal         :: Double    -- ^ Total (bounded by holographic)
  } deriving (Show)

-- | Decompose the extension deficit for a given observer and total system
decomposeDeficit :: QGObject  -- ^ Observer
                 -> Double    -- ^ Total system information
                 -> DeficitDecomposition
decomposeDeficit observer totalInfo =
  let -- Quantum contribution: entanglement with environment
      dQ = matterEntropy (qgMatter observer)

      -- Gravitational: depends on whether BH forms
      energy = matterEnergy (qgMatter observer)
      size   = regionSize (qgRegion observer)
      rs     = schwarzschildRadius energy
      dGrav  = if rs >= size
               then bekensteinHawkingEntropy (4 * pi * rs * rs)
               else 0

      -- Gauge: diffeomorphism orbit
      dim    = fromIntegral (regionDimension (qgRegion observer)) :: Double
      nCells = (size / planckLength) ** dim
      dGau   = if nCells > 1 then log nCells else 0

      -- Holographic bound
      area   = regionArea (qgRegion observer)
      dHolo  = bekensteinHawkingEntropy area

      -- Interaction term: quantum-gravity coupling
      -- Scales as sqrt(dQ * dGrav) when both are non-zero
      dInt   = if dQ > 0 && dGrav > 0 then sqrt (dQ * dGrav) else 0

      -- Total: sum of contributions, bounded by holographic
      total  = min (dQ + dGrav + dGau + dInt) dHolo

  in DeficitDecomposition
       { ddQuantum       = dQ
       , ddGravitational = dGrav
       , ddGauge         = dGau
       , ddHolographic   = dHolo
       , ddInteraction   = dInt
       , ddTotal         = total
       }

-- ============================================================
-- Convergence Under Expanding Observation
-- ============================================================

-- | A sequence of observers with expanding accessible regions
-- Returns the deficit at each step
convergenceSequence :: QGObject    -- ^ Initial observer
                   -> Double       -- ^ Total system information
                   -> Int          -- ^ Number of expansion steps
                   -> Double       -- ^ Growth factor per step
                   -> [(Double, Double)]  -- ^ (size, deficit) pairs
convergenceSequence initial totalInfo nSteps growthFactor =
  let step i =
        let factor = growthFactor ^ i
            expanded = initial
              { qgRegion = (qgRegion initial)
                  { regionSize   = regionSize (qgRegion initial) * factor
                  , regionArea   = regionArea (qgRegion initial) * factor ** 2
                  , regionVolume = regionVolume (qgRegion initial) * factor ** 3
                  }
              }
            deficit = ddTotal (decomposeDeficit expanded totalInfo)
            size    = regionSize (qgRegion expanded)
        in (size, deficit)
  in map step [0..nSteps-1]

-- | Check if the deficit sequence has converged (deficit ~ 0)
isConverged :: [(Double, Double)] -> Double -> Bool
isConverged [] _ = False
isConverged seq' threshold =
  let (_, lastDeficit) = last seq'
  in lastDeficit < threshold

-- ============================================================
-- Specific Physical Models
-- ============================================================

-- | Jaynes-Cummings model: single atom + cavity mode
-- The deficit is the mutual information between atom and field
jcModelDeficit :: Double  -- ^ Coupling strength g
              -> Double   -- ^ Detuning delta
              -> Double   -- ^ Time t
              -> Double   -- ^ Photon number n
              -> Double   -- ^ Extension deficit (nats)
jcModelDeficit g delta t n =
  let -- Rabi frequency
      omega = sqrt (g * g * (n + 1) + delta * delta / 4)
      -- Population inversion
      pExcited = (g * g * (n + 1) / (omega * omega)) * sin (omega * t) ** 2
      -- Von Neumann entropy of the atom
      sAtom = if pExcited > 0 && pExcited < 1
              then -pExcited * log pExcited - (1 - pExcited) * log (1 - pExcited)
              else 0
  in sAtom

-- | Spin chain model: N spins with nearest-neighbor coupling
-- The deficit for an observer having access to the first k spins
spinChainDeficit :: Int     -- ^ Total number of spins N
                -> Int     -- ^ Observer spins k
                -> Double  -- ^ Coupling J
                -> Double  -- ^ Extension deficit (nats)
spinChainDeficit nTotal kObs coupling =
  let -- For a critical spin chain, entanglement entropy scales as
      -- S ~ (c/3) * log(k) where c=1 for the Ising model
      centralCharge = 1.0  -- Ising model
      entropy = if kObs > 0 && kObs < nTotal
                then (centralCharge / 3) * log (fromIntegral kObs)
                else 0
  in entropy

-- | AdS/CFT model: boundary interval of length l
-- Returns the Ryu-Takayanagi entropy as the extension deficit
adsCftDeficit :: Double  -- ^ Central charge c
             -> Double   -- ^ Boundary interval length l
             -> Double   -- ^ UV cutoff epsilon
             -> Double   -- ^ AdS radius L_AdS
             -> Double   -- ^ Extension deficit (nats)
adsCftDeficit cCharge intervalL uvCutoff _adsRadius =
  let -- Ryu-Takayanagi formula
      rtEntropy = (cCharge / 3) * log (intervalL / uvCutoff)
  in rtEntropy

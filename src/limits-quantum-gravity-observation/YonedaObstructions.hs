{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Yoneda Obstructions for Quantum Gravity
--
-- Implements the three independent obstructions to Planck-scale observation:
-- 1. Gravitational epistemic horizon (black hole formation)
-- 2. Diffeomorphism gauge obstruction (background independence)
-- 3. Holographic saturation bound (Bekenstein-Hawking)
--
-- Reference: Long (2026), "Limits of Quantum Gravity Observation"

module YonedaObstructions
  ( -- * Obstruction types
    Obstruction(..)
  , ObstructionType(..)
    -- * Gravitational horizon
  , gravitationalObstruction
  , planckResolutionLimit
  , transPlanckianCensorship
    -- * Diffeomorphism gauge
  , diffeomorphismObstruction
  , relationalObservableCount
  , problemOfTimeContribution
    -- * Holographic bound
  , holographicObstruction
  , ryuTakayanagiDeficit
  , quantumExtremalSurface
    -- * Composition
  , composeObstructions
  , totalObstruction
    -- * Scale analysis
  , ScaleRegime(..)
  , classifyRegime
  , regimeAnalysis
  ) where

import QGMeasurementCategory

-- ============================================================
-- Obstruction Framework
-- ============================================================

-- | Types of obstructions
data ObstructionType
  = GravitationalHorizon    -- ^ Black hole formation
  | DiffeomorphismGauge     -- ^ Background independence
  | HolographicSaturation   -- ^ Bekenstein-Hawking bound
  deriving (Show, Eq, Enum, Bounded)

-- | An obstruction with its quantitative measure
data Obstruction = Obstruction
  { obstructionType       :: ObstructionType
  , obstructionMagnitude  :: Double    -- ^ Bits of information lost
  , obstructionSaturated  :: Bool      -- ^ Whether the bound is tight
  , obstructionDescription :: String
  } deriving (Show)

-- ============================================================
-- Gravitational Horizon Obstruction
-- ============================================================

-- | Compute the gravitational horizon obstruction
gravitationalObstruction :: QGObject -> Obstruction
gravitationalObstruction obj =
  let energy = matterEnergy (qgMatter obj)
      size   = regionSize (qgRegion obj)
      rs     = schwarzschildRadius energy
      -- Information hidden behind horizon (if any)
      magnitude
        | rs >= size = bekensteinHawkingEntropy (4 * pi * rs * rs)
        | otherwise  = 0
      saturated = rs >= size
  in Obstruction
       { obstructionType        = GravitationalHorizon
       , obstructionMagnitude   = magnitude
       , obstructionSaturated   = saturated
       , obstructionDescription =
           if saturated
           then "Black hole formed: r_s = " ++ show rs ++ " >= L = " ++ show size
           else "Sub-threshold: r_s = " ++ show rs ++ " < L = " ++ show size
       }

-- | The minimum resolvable length scale
-- delta_x >= max(hbar/E, 2 G_N E / c^4) >= 2 l_P
planckResolutionLimit :: Double -> Double
planckResolutionLimit probeEnergy =
  let quantumLimit = hbar * cLight / probeEnergy
      gravLimit    = schwarzschildRadius probeEnergy
  in max quantumLimit gravLimit

-- | Optimal probe energy minimizing total uncertainty
optimalProbeEnergy :: Double
optimalProbeEnergy = planckEnergy / sqrt 2

-- | Check trans-Planckian censorship: is the energy density above Planck?
transPlanckianCensorship :: QGObject -> Bool
transPlanckianCensorship obj =
  let energy = matterEnergy (qgMatter obj)
      volume = regionVolume (qgRegion obj)
      planckDensity = planckEnergy / planckLength ^ (3 :: Int)
  in (energy / volume) > planckDensity

-- ============================================================
-- Diffeomorphism Gauge Obstruction
-- ============================================================

-- | Compute the diffeomorphism gauge obstruction
diffeomorphismObstruction :: QGObject -> Obstruction
diffeomorphismObstruction obj =
  let dim  = fromIntegral (regionDimension (qgRegion obj)) :: Double
      size = regionSize (qgRegion obj)
      -- Estimate: diffeomorphism orbit volume ~ (L/l_P)^d
      nCells = (size / planckLength) ** dim
      magnitude = if nCells > 1 then log nCells / log 2 else 0
      -- Saturated when all points are gauge-related (quantum foam)
      saturated = size <= planckLength
  in Obstruction
       { obstructionType        = DiffeomorphismGauge
       , obstructionMagnitude   = magnitude
       , obstructionSaturated   = saturated
       , obstructionDescription =
           "Diffeomorphism orbit: ~" ++ show (round nCells :: Integer)
           ++ " Planck cells, " ++ show (round magnitude :: Integer) ++ " bits"
       }

-- | Count of relational (diffeomorphism-invariant) observables
-- relative to total kinematical observables
relationalObservableCount :: QGObject -> (Int, Int)
relationalObservableCount obj =
  let dim    = regionDimension (qgRegion obj)  -- Spatial dimension (typically 3)
      hilbD  = matterDimHilb (qgMatter obj)
      -- Kinematical observables: dim(H)^2 - 1
      kinematical = hilbD * hilbD - 1
      -- Physical (diffeo-invariant): reduced by diffeomorphism group
      -- In 4D gravity, there are 4 constraints per spacetime point:
      --   1 Hamiltonian constraint + 3 spatial diffeomorphism constraints
      -- nPoints is the number of spatial lattice points; each carries
      -- 4 gauge parameters (1 temporal + 3 spatial diffeomorphisms)
      nPoints     = round ((regionSize (qgRegion obj) / planckLength)
                    ** fromIntegral dim) :: Int
      gaugeParams = 4 * nPoints  -- 4 = (dim+1) constraints per point in 3+1 D
      physical    = max 1 (kinematical - gaugeParams)
  in (physical, kinematical)

-- | Problem of time contribution: how much temporal information is gauge
problemOfTimeContribution :: QGObject -> Double
problemOfTimeContribution obj =
  let size = regionSize (qgRegion obj)
      -- Temporal resolution: size/c
      nTemporalSlices = size / (cLight * planckTime)
      -- Each slice carries one bit of "when"
  in if nTemporalSlices > 1 then log nTemporalSlices / log 2 else 0

-- ============================================================
-- Holographic Saturation Bound
-- ============================================================

-- | Compute the holographic saturation obstruction
holographicObstruction :: QGObject -> Obstruction
holographicObstruction obj =
  let area = regionArea (qgRegion obj)
      sbh  = bekensteinHawkingEntropy area
      -- Convert to bits
      magnitude = sbh / log 2
      -- Saturated when entropy equals BH entropy
      actualEntropy = matterEntropy (qgMatter obj)
      saturated = actualEntropy >= sbh
  in Obstruction
       { obstructionType        = HolographicSaturation
       , obstructionMagnitude   = magnitude
       , obstructionSaturated   = saturated
       , obstructionDescription =
           "S_BH = " ++ show sbh ++ " nats, actual S = "
           ++ show actualEntropy ++ " nats"
       }

-- | Ryu-Takayanagi deficit for a boundary subregion in AdS/CFT
-- Given boundary interval length l and UV cutoff epsilon
ryuTakayanagiDeficit :: Double -> Double -> Double -> Double
ryuTakayanagiDeficit centralCharge intervalLength uvCutoff =
  (centralCharge / 3) * log (intervalLength / uvCutoff)

-- | Quantum extremal surface generalized deficit
-- Area term + bulk entanglement
quantumExtremalSurface :: Double -> Double -> Double
quantumExtremalSurface surfaceArea bulkEntropy =
  bekensteinHawkingEntropy surfaceArea + bulkEntropy

-- ============================================================
-- Composition of Obstructions
-- ============================================================

-- | Compose multiple obstructions, respecting the holographic bound
composeObstructions :: [Obstruction] -> ExtensionDeficit
composeObstructions obstructions =
  let findObs t = case filter (\o -> obstructionType o == t) obstructions of
                    (o:_) -> obstructionMagnitude o
                    []    -> 0
      dQ    = findObs GravitationalHorizon  -- Reusing for quantum deficit
      dGrav = findObs GravitationalHorizon
      dGau  = findObs DiffeomorphismGauge
      dHolo = findObs HolographicSaturation
      total = min (dQ + dGrav + dGau) dHolo
  in ExtensionDeficit
       { deficitQuantum       = 0  -- Not directly from obstructions
       , deficitGravitational = dGrav
       , deficitGauge         = dGau
       , deficitHolographic   = dHolo
       , deficitTotal         = total
       }

-- | Compute the total obstruction for an object
totalObstruction :: QGObject -> [Obstruction]
totalObstruction obj =
  [ gravitationalObstruction obj
  , diffeomorphismObstruction obj
  , holographicObstruction obj
  ]

-- ============================================================
-- Scale Regime Analysis
-- ============================================================

-- | Physical scale regimes
data ScaleRegime
  = Classical          -- ^ L >> l_P, E << E_P
  | Semiclassical      -- ^ L ~ l_P, E ~ E_P
  | PlanckScale        -- ^ L ~ l_P, E ~ E_P
  | TransPlanckian     -- ^ L < l_P, E > E_P
  deriving (Show, Eq, Ord)

-- | Classify the scale regime of an object
classifyRegime :: QGObject -> ScaleRegime
classifyRegime obj =
  let size   = regionSize (qgRegion obj)
      energy = matterEnergy (qgMatter obj)
      ratio  = size / planckLength
      eRatio = energy / planckEnergy
  in case () of
       _ | ratio > 100 && eRatio < 0.01 -> Classical
         | ratio > 1   && eRatio < 1    -> Semiclassical
         | ratio <= 1  && eRatio >= 1   -> TransPlanckian
         | otherwise                    -> PlanckScale

-- | Full regime analysis: regime + obstructions + deficit
regimeAnalysis :: QGObject -> (ScaleRegime, [Obstruction], ExtensionDeficit)
regimeAnalysis obj =
  let regime = classifyRegime obj
      obsts  = totalObstruction obj
      deficit = composeObstructions obsts
  in (regime, obsts, deficit)

-- ============================================================
-- QG Approach-Specific Obstructions
-- ============================================================

-- | Loop Quantum Gravity: area gap
lqgAreaGap :: Double -> Double
lqgAreaGap barberoImmirzi =
  4 * sqrt 3 * pi * barberoImmirzi * planckLength ^ (2 :: Int)

-- | String theory: T-duality identification
-- R and alpha'/R are equivalent, so minimum scale is sqrt(alpha')
stringMinScale :: Double -> Double
stringMinScale alphaPrime = sqrt alphaPrime

-- | Asymptotic safety: running Newton's constant
-- G(k) ~ G* / (1 + g* k^2 / k_P^2) for UV fixed point
asRunningG :: Double -> Double -> Double -> Double
asRunningG gStar kScale kPlanck =
  newtonG / (1 + gStar * (kScale / kPlanck) ^ (2 :: Int))

-- | Causal set: number of elements determines information bound
causalSetBound :: Int -> Double
causalSetBound nElements = fromIntegral nElements * log 2

{-# LANGUAGE GADTs #-}

-- | PageCurve.hs
--
-- Simulation of the entanglement entropy as a function of retarded
-- time u, showing the Page curve behavior and the island transition.
--
-- The Page curve rises linearly before the Page time u_P (Hawking's
-- calculation), then decreases after u_P when the island formula
-- takes over. In the Yoneda framework, this corresponds to a
-- structural transition in the representable functor (Theorem 7.1):
--
--   u < u_P : Delta(S_Rad(u)) increasing (no island)
--   u > u_P : Delta(S_Rad(u)) decreasing (island dominates)
--   u = u_evap : Delta = 0 (full recovery, unitarity preserved)
--
-- Reference: "The Black Hole Information Paradox from the Yoneda
-- Constraint Perspective" (Long, 2026), Section 7.

module PageCurve
  ( -- * Black hole parameters
    BlackHoleParams(..)
  , defaultParams
    -- * Page curve computation
  , PageCurvePoint(..)
  , pageCurve
  , pageTime
  , evaporationTime
    -- * Entropy formulas
  , hawkingEntropy
  , islandEntropy
  , totalEntropy
    -- * Extension deficit evolution
  , deficitEvolution
    -- * Phase detection
  , Phase(..)
  , currentPhase
  ) where

-- | Parameters for the black hole evaporation model.
data BlackHoleParams = BlackHoleParams
  { initialMass :: Double
    -- ^ Initial black hole mass M_0 (in Planck units).
  , hawkingRate :: Double
    -- ^ Rate of Hawking emission dM/du (simplified model).
  , beckensteinHawkingEntropy :: Double
    -- ^ Initial Bekenstein-Hawking entropy S_BH = A/4G_N.
  , numModes :: Int
    -- ^ Number of radiation modes for simulation.
  } deriving (Show)

-- | Default parameters for a solar-mass-scale black hole
-- (scaled for numerical tractability).
defaultParams :: BlackHoleParams
defaultParams = BlackHoleParams
  { initialMass = 100.0
  , hawkingRate  = 0.1
  , beckensteinHawkingEntropy = 100.0
  , numModes = 200
  }

-- | A point on the Page curve.
data PageCurvePoint = PageCurvePoint
  { time :: Double
    -- ^ Retarded time u.
  , radiationEntropy :: Double
    -- ^ S(Rad(u)): entanglement entropy of the radiation.
  , blackHoleEntropy :: Double
    -- ^ S_BH(u): Bekenstein-Hawking entropy of the remaining BH.
  , extensionDeficitValue :: Double
    -- ^ Delta(S_Rad(u)): the Kan extension deficit at time u.
  , phase :: Phase
    -- ^ Which phase of the Page curve we are in.
  } deriving (Show)

-- | Phases of the Page curve, corresponding to the
-- presheaf transition described in Theorem 7.1.
data Phase
  = PrePage
    -- ^ u < u_P: Hawking radiation thermal, deficit increasing.
    -- The representable functor of S_Rad(u) grows but the
    -- Kan extension deficit worsens (Theorem 7.1, part i).
  | PostPage
    -- ^ u > u_P: Island formula dominates, deficit decreasing.
    -- The radiation observer's functor begins to "see" interior
    -- information through entanglement (Theorem 7.1, part ii).
  | FullyEvaporated
    -- ^ u = u_evap: Black hole gone, radiation pure, Delta = 0.
    -- Full information recovery if unitarity holds
    -- (Theorem 7.1, part iii).
  deriving (Eq, Show)

-- | Compute the Page time u_P: the time at which the radiation
-- subsystem becomes larger than the black hole subsystem.
-- At u_P, S(Rad) = S_BH, and the entropy begins to decrease.
pageTime :: BlackHoleParams -> Double
pageTime params =
  -- Page time is when half the initial entropy has been radiated.
  -- In our simplified model: u_P = S_BH_0 / (2 * rate).
  beckensteinHawkingEntropy params / (2.0 * hawkingRate params)

-- | Compute the evaporation time u_evap.
evaporationTime :: BlackHoleParams -> Double
evaporationTime params =
  initialMass params / hawkingRate params

-- | Hawking entropy of the radiation at time u (before Page time).
-- In the Hawking calculation (no islands), the entropy grows
-- linearly: S_Hawking(u) = rate * u.
hawkingEntropy :: BlackHoleParams -> Double -> Double
hawkingEntropy params u =
  min (hawkingRate params * u) (beckensteinHawkingEntropy params)

-- | Island entropy at time u (after Page time).
-- With the island contribution:
-- S_island(u) = Area(partial I*(u)) / 4G_N + S_bulk(Rad union I*)
-- which decreases after the Page time.
islandEntropy :: BlackHoleParams -> Double -> Double
islandEntropy params u =
  let uEvap = evaporationTime params
      sBH0  = beckensteinHawkingEntropy params
      -- Remaining BH entropy decreases linearly
      remaining = max 0.0 (sBH0 - hawkingRate params * u)
  in remaining

-- | Total entanglement entropy of the radiation at time u.
-- This is the Page curve (Theorem 7.3):
--   S(Rad(u)) = min(S_Hawking(u), S_island(u))
-- The min implements the island formula's saddle-point selection.
totalEntropy :: BlackHoleParams -> Double -> Double
totalEntropy params u =
  let sHawk   = hawkingEntropy params u
      sIsland = islandEntropy params u
  in min sHawk sIsland

-- | Determine the current phase of the Page curve.
currentPhase :: BlackHoleParams -> Double -> Phase
currentPhase params u
  | u >= evaporationTime params = FullyEvaporated
  | u >= pageTime params        = PostPage
  | otherwise                   = PrePage

-- | Compute the full Page curve as a list of data points.
--
-- The curve demonstrates:
-- - Pre-Page: entropy rises (thermal Hawking radiation, no correlations)
-- - Page time: entropy peaks (transition point, Theorem 7.1)
-- - Post-Page: entropy falls (island formula, deficit decreasing)
-- - Evaporation: entropy -> 0 (pure state, unitarity preserved)
pageCurve :: BlackHoleParams -> Int -> [PageCurvePoint]
pageCurve params nPoints =
  let uEvap = evaporationTime params
      dt    = uEvap / fromIntegral nPoints
      times = [fromIntegral i * dt | i <- [0..nPoints]]
  in map (makePoint params) times

-- | Create a single Page curve data point.
makePoint :: BlackHoleParams -> Double -> PageCurvePoint
makePoint params u =
  let sRad = totalEntropy params u
      sBH  = max 0.0 (beckensteinHawkingEntropy params - hawkingRate params * u)
      -- Extension deficit is proportional to the entanglement
      -- entropy (Proposition 6.4):
      -- Delta /= 0 iff S(rho_Rad) > 0
      deficit = sRad / beckensteinHawkingEntropy params
      ph = currentPhase params u
  in PageCurvePoint u sRad sBH deficit ph

-- | Evolution of the Kan extension deficit over time.
-- Returns (time, deficit) pairs showing the rise and fall
-- corresponding to the presheaf transition (Theorem 7.1).
deficitEvolution :: BlackHoleParams -> Int -> [(Double, Double)]
deficitEvolution params nPoints =
  map (\p -> (time p, extensionDeficitValue p)) (pageCurve params nPoints)

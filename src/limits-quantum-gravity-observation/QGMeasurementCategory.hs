{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- | Quantum Gravitational Measurement Category
--
-- This module implements the categorical structures from
-- "Limits of Quantum Gravity Observation: A Yoneda Constraint Analysis"
-- (Long, 2026). It models the quantum gravitational measurement category
-- Meas_QG, the three obstructions to Planck-scale observation, and the
-- computation of extension deficits.

module QGMeasurementCategory
  ( -- * Core types
    SpacetimeRegion(..)
  , MetricState(..)
  , MatterState(..)
  , QGObject(..)
  , QGMorphism(..)
  , MeasCategory(..)
    -- * Epistemic horizons
  , EpistemicHorizon(..)
  , gravitationalHorizon
  , isAccessible
    -- * Extension deficit
  , ExtensionDeficit(..)
  , quantumDeficit
  , gravitationalDeficit
  , gaugeDeficit
  , holographicDeficit
  , totalDeficit
    -- * Physical constants and scales
  , planckLength
  , planckEnergy
  , planckTime
  , schwarzschildRadius
  , bekensteinHawkingEntropy
    -- * Observational scenarios
  , ObservationalScenario(..)
  , evaluateScenario
  ) where

import Data.List (nub)

-- ============================================================
-- Physical Constants (in natural units where c = 1)
-- ============================================================

-- | Planck length in meters
planckLength :: Double
planckLength = 1.616255e-35

-- | Planck energy in joules
planckEnergy :: Double
planckEnergy = 1.956e9  -- ~1.22e19 GeV in joules

-- | Planck time in seconds
planckTime :: Double
planckTime = 5.391247e-44

-- | Newton's gravitational constant in SI
newtonG :: Double
newtonG = 6.67430e-11

-- | Reduced Planck constant
hbar :: Double
hbar = 1.054571817e-34

-- | Speed of light
cLight :: Double
cLight = 2.99792458e8

-- | Schwarzschild radius for a given energy (in SI)
schwarzschildRadius :: Double -> Double
schwarzschildRadius energy = 2 * newtonG * energy / (cLight ^ (4 :: Int))

-- | Bekenstein-Hawking entropy for a given area (in Planck units)
bekensteinHawkingEntropy :: Double -> Double
bekensteinHawkingEntropy area = area / (4 * newtonG * hbar)

-- ============================================================
-- Core Category Types
-- ============================================================

-- | A spacetime region characterized by size and topology
data SpacetimeRegion = SpacetimeRegion
  { regionSize       :: Double   -- ^ Characteristic length scale (meters)
  , regionDimension  :: Int      -- ^ Spatial dimension (typically 3)
  , regionArea       :: Double   -- ^ Boundary area (meters^2)
  , regionVolume     :: Double   -- ^ Volume (meters^3)
  } deriving (Show, Eq)

-- | The metric state on a region (simplified)
data MetricState
  = FlatMetric                      -- ^ Minkowski spacetime
  | SchwarzschildMetric Double      -- ^ Schwarzschild with mass parameter
  | DeSitterMetric Double           -- ^ de Sitter with cosmological constant
  | QuantumSuperposition [MetricState] -- ^ Quantum superposition of metrics
  | SpinNetworkState Int [Double]   -- ^ LQG: graph with n vertices, spins
  | CausalSetState Int              -- ^ Causal set: n elements
  deriving (Show, Eq)

-- | The matter state on a region
data MatterState = MatterState
  { matterEnergy    :: Double    -- ^ Total energy
  , matterEntropy   :: Double    -- ^ Von Neumann entropy
  , matterDimHilb   :: Int       -- ^ Hilbert space dimension
  } deriving (Show, Eq)

-- | An object in Meas_QG: a triple (S, g_S, rho_S)
data QGObject = QGObject
  { qgRegion  :: SpacetimeRegion
  , qgMetric  :: MetricState
  , qgMatter  :: MatterState
  } deriving (Show, Eq)

-- | A morphism in Meas_QG: diffeomorphism-compatible quantum channel
data QGMorphism = QGMorphism
  { morphSource :: QGObject
  , morphTarget :: QGObject
  , morphIsDiffeoCompatible :: Bool
  , morphIsStatePreserving  :: Bool
  } deriving (Show, Eq)

-- | The measurement category
data MeasCategory = MeasCategory
  { catObjects   :: [QGObject]
  , catMorphisms :: [QGMorphism]
  } deriving (Show)

-- ============================================================
-- Epistemic Horizons
-- ============================================================

-- | Epistemic horizon: the accessible subcategory
data EpistemicHorizon = EpistemicHorizon
  { horizonObjects    :: [QGObject]    -- ^ Accessible objects
  , horizonBoundary   :: [QGObject]    -- ^ Boundary objects
  , horizonExcluded   :: [QGObject]    -- ^ Objects beyond horizon
  , maxProbeEnergy    :: Double        -- ^ Maximum probe energy before BH
  , minResolvableScale :: Double       -- ^ Minimum resolvable length
  } deriving (Show)

-- | Compute the gravitational epistemic horizon for an observer
gravitationalHorizon :: QGObject -> MeasCategory -> EpistemicHorizon
gravitationalHorizon observer cat =
  let size = regionSize (qgRegion observer)
      -- Schwarzschild bound: E < c^4 L / (2 G_N)
      maxE = cLight ^ (4 :: Int) * size / (2 * newtonG)
      -- Minimum resolvable scale: max(hbar/E, 2 G_N E / c^4)
      minScale = 2 * planckLength
      -- Partition objects by accessibility
      accessible = filter (isAccessible maxE) (catObjects cat)
      boundary   = filter (isOnBoundary maxE) (catObjects cat)
      excluded   = filter (not . isAccessible maxE) (catObjects cat)
  in EpistemicHorizon
       { horizonObjects     = accessible
       , horizonBoundary    = boundary
       , horizonExcluded    = excluded
       , maxProbeEnergy     = maxE
       , minResolvableScale = minScale
       }

-- | Check if an object is accessible (below Schwarzschild threshold)
isAccessible :: Double -> QGObject -> Bool
isAccessible maxE obj =
  let energy = matterEnergy (qgMatter obj)
      size   = regionSize (qgRegion obj)
      rs     = schwarzschildRadius energy
  in energy < maxE && rs < size

-- | Check if an object is on the epistemic boundary
isOnBoundary :: Double -> QGObject -> Bool
isOnBoundary maxE obj =
  let energy = matterEnergy (qgMatter obj)
      size   = regionSize (qgRegion obj)
      rs     = schwarzschildRadius energy
      -- Within 10% of the threshold
      ratio  = energy / maxE
  in ratio > 0.9 && ratio <= 1.0 && rs < size

-- ============================================================
-- Extension Deficit
-- ============================================================

-- | The extension deficit decomposition
data ExtensionDeficit = ExtensionDeficit
  { deficitQuantum       :: Double  -- ^ Delta_Q: entanglement contribution
  , deficitGravitational :: Double  -- ^ Delta_grav: BH formation
  , deficitGauge         :: Double  -- ^ Delta_gauge: diffeomorphism quotient
  , deficitHolographic   :: Double  -- ^ Delta_holo: Bekenstein-Hawking bound
  , deficitTotal         :: Double  -- ^ Sum of contributions
  } deriving (Show)

-- | Quantum contribution: von Neumann entropy of reduced state
quantumDeficit :: QGObject -> Double
quantumDeficit obj = matterEntropy (qgMatter obj)

-- | Gravitational contribution: entropy behind any formed horizons
gravitationalDeficit :: QGObject -> Double
gravitationalDeficit obj =
  let energy = matterEnergy (qgMatter obj)
      size   = regionSize (qgRegion obj)
      rs     = schwarzschildRadius energy
  in if rs >= size
     then bekensteinHawkingEntropy (4 * pi * rs * rs)
     else 0

-- | Gauge contribution: log of diffeomorphism orbit volume
-- Estimated as log(Vol(Diff(S))) ~ dimension * log(size/l_P)
-- NOTE: This estimate assumes a discrete UV cutoff at the Planck scale,
-- consistent with LQG and Causal Set approaches. In Asymptotic Safety,
-- the running of G_N may modify this estimate at high energies.
gaugeDeficit :: QGObject -> Double
gaugeDeficit obj =
  let dim  = fromIntegral (regionDimension (qgRegion obj)) :: Double
      size = regionSize (qgRegion obj)
      -- Number of Planck-sized cells
      nCells = (size / planckLength) ** dim
  in if nCells > 1 then log nCells else 0

-- | Holographic contribution: A/(4 G hbar)
holographicDeficit :: QGObject -> Double
holographicDeficit obj =
  let area = regionArea (qgRegion obj)
  in bekensteinHawkingEntropy area

-- | Total extension deficit
totalDeficit :: QGObject -> ExtensionDeficit
totalDeficit obj =
  let dQ    = quantumDeficit obj
      dGrav = gravitationalDeficit obj
      dGau  = gaugeDeficit obj
      dHolo = holographicDeficit obj
      -- Holographic bound: total <= holographic
      total = min (dQ + dGrav + dGau) dHolo
  in ExtensionDeficit
       { deficitQuantum       = dQ
       , deficitGravitational = dGrav
       , deficitGauge         = dGau
       , deficitHolographic   = dHolo
       , deficitTotal         = total
       }

-- ============================================================
-- Representable Functor (simplified model)
-- ============================================================

-- | The representable functor yo^(S, g, rho) evaluated on an object X
-- Returns the number of independent morphisms (channels) from S to X
representableFunctor :: QGObject -> QGObject -> Int
representableFunctor source target
  | not (isEnergyCompatible source target) = 0  -- Beyond horizon
  | isDiffeoEquivalent source target = 1         -- Gauge-identified
  | otherwise = min hilbDim holoBound
  where
    hilbDim   = matterDimHilb (qgMatter source)
    holoBound = floor (exp (holographicDeficit source))
    isEnergyCompatible s t =
      let eS = matterEnergy (qgMatter s)
          eT = matterEnergy (qgMatter t)
          sizeS = regionSize (qgRegion s)
      in eT < cLight ^ (4 :: Int) * sizeS / (2 * newtonG)
    isDiffeoEquivalent s t =
      qgMetric s == qgMetric t && qgMatter s == qgMatter t

-- | Dimension of the representable functor (total components)
representableDimension :: QGObject -> MeasCategory -> Int
representableDimension observer cat =
  sum [representableFunctor observer x | x <- catObjects cat]

-- ============================================================
-- Observational Scenarios
-- ============================================================

-- | An observational scenario with its parameters
data ObservationalScenario = ObservationalScenario
  { scenarioName        :: String
  , scenarioObserver    :: QGObject
  , scenarioProbeEnergy :: Double    -- ^ Energy of the probe
  , scenarioProbeScale  :: Double    -- ^ Length scale being probed
  } deriving (Show)

-- | Evaluate an observational scenario, computing the deficit
evaluateScenario :: ObservationalScenario -> ExtensionDeficit
evaluateScenario scenario =
  let obs    = scenarioObserver scenario
      energy = scenarioProbeEnergy scenario
      scale  = scenarioProbeScale scenario
      -- Modify observer to include probe energy
      probed = obs { qgMatter = (qgMatter obs)
                       { matterEnergy = matterEnergy (qgMatter obs) + energy } }
  in totalDeficit probed

-- ============================================================
-- Example Scenarios
-- ============================================================

-- | LIGO detector scenario
ligoScenario :: ObservationalScenario
ligoScenario = ObservationalScenario
  { scenarioName = "LIGO Gravitational Wave Detector"
  , scenarioObserver = QGObject
      { qgRegion = SpacetimeRegion
          { regionSize = 4000       -- 4 km arms
          , regionDimension = 3
          , regionArea = 4 * pi * 4000 ^ (2 :: Int)
          , regionVolume = (4/3) * pi * 4000 ^ (3 :: Int)
          }
      , qgMetric = FlatMetric
      , qgMatter = MatterState
          { matterEnergy = 1e-20    -- ~photon energy sensitivity
          , matterEntropy = 10      -- Low entropy classical detector
          , matterDimHilb = 1024    -- Effective Hilbert space dim
          }
      }
  , scenarioProbeEnergy = 1e-20
  , scenarioProbeScale  = 1e-18    -- ~attometer
  }

-- | BMV tabletop experiment
bmvScenario :: ObservationalScenario
bmvScenario = ObservationalScenario
  { scenarioName = "BMV Gravitational Entanglement"
  , scenarioObserver = QGObject
      { qgRegion = SpacetimeRegion
          { regionSize = 1e-3       -- 1 mm
          , regionDimension = 3
          , regionArea = 4 * pi * (1e-3) ^ (2 :: Int)
          , regionVolume = (4/3) * pi * (1e-3) ^ (3 :: Int)
          }
      , qgMetric = FlatMetric
      , qgMatter = MatterState
          { matterEnergy = 1e-14    -- ~nanogram masses
          , matterEntropy = 1       -- Near-pure quantum state
          , matterDimHilb = 4       -- Two qubits
          }
      }
  , scenarioProbeEnergy = 1e-14
  , scenarioProbeScale  = 1e-3
  }

-- | Planck-scale probe (thought experiment)
planckScenario :: ObservationalScenario
planckScenario = ObservationalScenario
  { scenarioName = "Planck Scale Probe"
  , scenarioObserver = QGObject
      { qgRegion = SpacetimeRegion
          { regionSize = planckLength
          , regionDimension = 3
          , regionArea = 4 * pi * planckLength ^ (2 :: Int)
          , regionVolume = (4/3) * pi * planckLength ^ (3 :: Int)
          }
      , qgMetric = QuantumSuperposition [FlatMetric, SchwarzschildMetric 1e-8]
      , qgMatter = MatterState
          { matterEnergy = planckEnergy
          , matterEntropy = 1       -- O(1) bits at Planck scale
          , matterDimHilb = 2       -- Minimal
          }
      }
  , scenarioProbeEnergy = planckEnergy
  , scenarioProbeScale  = planckLength
  }

-- | Black hole observation
blackHoleScenario :: ObservationalScenario
blackHoleScenario = ObservationalScenario
  { scenarioName = "Stellar Black Hole Observation"
  , scenarioObserver = QGObject
      { qgRegion = SpacetimeRegion
          { regionSize = 3e4        -- 30 km (~ 10 solar mass BH)
          , regionDimension = 3
          , regionArea = 4 * pi * (3e4) ^ (2 :: Int)
          , regionVolume = (4/3) * pi * (3e4) ^ (3 :: Int)
          }
      , qgMetric = SchwarzschildMetric (2e31)  -- ~10 solar masses
      , qgMatter = MatterState
          { matterEnergy = 2e31 * cLight ^ (2 :: Int)
          , matterEntropy = 1e77    -- ~Bekenstein-Hawking entropy
          , matterDimHilb = maxBound
          }
      }
  , scenarioProbeEnergy = 1e-15
  , scenarioProbeScale  = 3e4
  }

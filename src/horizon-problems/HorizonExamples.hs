{-# LANGUAGE ScopedTypeVariables #-}

-- | HorizonExamples.hs
-- Concrete examples demonstrating horizon constructions on lattice spacetimes.
-- Each example constructs a discrete measurement category, computes the
-- accessible subcategory for a given horizon, and evaluates the Kan extension deficit.
--
-- Reference: "Horizon Problems and the Yoneda Constraint" (Long, 2026)

module HorizonExamples
  ( -- * Result types
    HorizonResult(..)
  , BracketResult(..)

    -- * Examples
  , rindlerExample
  , blackHoleExample
  , deSitterExample
  , cosmoExample
  , kanBracketExample
  ) where

import CausalCategory

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- ============================================================
-- Result Types
-- ============================================================

-- | Result of a horizon analysis.
data HorizonResult = HorizonResult
  { hrObserverLabel      :: !String
  , hrTotalObjects       :: !Int
  , hrAccessibleObjects  :: !Int
  , hrTotalMorphisms     :: !Int
  , hrAccessibleMorphisms :: !Int
  , hrZeroOverCat        :: !Int    -- ^ Objects with empty over-category
  , hrDeficitMagnitude   :: !Double
  , hrFaithful           :: !Bool   -- ^ Is the inclusion faithful?
  , hrFull               :: !Bool   -- ^ Is the inclusion full?
  } deriving (Show)

-- | Result of a Kan extension bracket analysis.
data BracketResult = BracketResult
  { brObserverLabel    :: !String
  , brLeftKanTotal     :: !Int
  , brRightKanTotal    :: !Int
  , brBracketWidth     :: !Int
  , brObjectsInBracket :: !Int
  } deriving (Show)


-- ============================================================
-- Lattice Spacetime Construction
-- ============================================================

-- | Create a (1+1)-dimensional lattice spacetime (time x space_1) with n points
-- in each dimension. The remaining spatial coordinates (space_2, space_3) are
-- set to zero. This is a lower-dimensional toy model suitable for demonstrating
-- Rindler, Schwarzschild, and de Sitter horizon constructions.
-- Time ranges from -n/2 to n/2, space_1 from -n/2 to n/2.
lattice2D :: Int -> Set SpacetimePoint
lattice2D n = Set.fromList
  [ STP (fromIntegral t) (fromIntegral x) 0 0
  | t <- [-(n `div` 2) .. (n `div` 2)]
  , x <- [-(n `div` 2) .. (n `div` 2)]
  ]

-- | Build a measurement category from a set of spacetime points.
-- Objects: each point becomes a region (singleton). States are labeled by position.
-- Morphisms: causal embeddings between causally related points.
buildMeasCategory :: Set SpacetimePoint -> MeasCategory
buildMeasCategory pts = MeasCategory
  { mcObjects   = objs
  , mcMorphisms = morphs
  }
  where
    ptsList = Set.toList pts
    -- Each point becomes a measurement object
    mkObj p = MeasObject
      { moRegion   = CausalRegion (hash p) (Set.singleton p) (show p)
      , moState    = "state_" ++ show (stTime p) ++ "_" ++ show (stSpace1 p)
      , moObserver = Nothing
      }

    objs = Set.fromList $ map mkObj ptsList

    -- Morphisms: one for each causal relation (p causally before q)
    morphs = Set.fromList
      [ MeasMorphism (mkObj p) (mkObj q) ("causal_" ++ show p ++ "->" ++ show q)
      | p <- ptsList
      , q <- ptsList
      , p /= q
      , let dt = stTime q - stTime p
            dx = stSpace1 q - stSpace1 p
      , dt > 0  -- q is in the future of p
      , abs dx <= dt  -- within the lightcone (c=1)
      ]

    -- Simple hash for region IDs
    hash p = round (stTime p * 1000 + stSpace1 p * 100
                  + stSpace2 p * 10 + stSpace3 p)


-- ============================================================
-- Example: Rindler Horizon
-- ============================================================

-- | Rindler horizon example on an n x n lattice.
-- The right Rindler wedge is x > |t|.
rindlerExample :: Int -> HorizonResult
rindlerExample n =
  let pts = lattice2D n
      cat = buildMeasCategory pts
      obs = rindlerHorizon pts
      sub = accessibleSubcategory cat obs
      deficit = extensionDeficit cat sub
      kanLeft = kanExtensionLeft cat sub
      zeroOC = Map.size $ Map.filter (== 0) kanLeft
  in HorizonResult
    { hrObserverLabel      = hoLabel obs
    , hrTotalObjects       = Set.size (mcObjects cat)
    , hrAccessibleObjects  = Set.size (mcObjects sub)
    , hrTotalMorphisms     = Set.size (mcMorphisms cat)
    , hrAccessibleMorphisms = Set.size (mcMorphisms sub)
    , hrZeroOverCat        = zeroOC
    , hrDeficitMagnitude   = deficitMagnitude deficit
    , hrFaithful           = True   -- Inclusion is always faithful
    , hrFull               = Set.size (mcObjects sub) == Set.size (mcObjects cat)
    }


-- ============================================================
-- Example: Black Hole Event Horizon
-- ============================================================

-- | Black hole event horizon example.
-- Observer is at r = 2*r_H, horizon at r = r_H.
blackHoleExample :: Int -> Double -> HorizonResult
blackHoleExample n rH =
  let pts = lattice2D n
      cat = buildMeasCategory pts
      obs = eventHorizon rH pts
      sub = accessibleSubcategory cat obs
      deficit = extensionDeficit cat sub
      kanLeft = kanExtensionLeft cat sub
      zeroOC = Map.size $ Map.filter (== 0) kanLeft
  in HorizonResult
    { hrObserverLabel      = hoLabel obs
    , hrTotalObjects       = Set.size (mcObjects cat)
    , hrAccessibleObjects  = Set.size (mcObjects sub)
    , hrTotalMorphisms     = Set.size (mcMorphisms cat)
    , hrAccessibleMorphisms = Set.size (mcMorphisms sub)
    , hrZeroOverCat        = zeroOC
    , hrDeficitMagnitude   = deficitMagnitude deficit
    , hrFaithful           = True
    , hrFull               = Set.size (mcObjects sub) == Set.size (mcObjects cat)
    }


-- ============================================================
-- Example: De Sitter Horizon
-- ============================================================

-- | De Sitter horizon example.
-- Observer at origin, horizon at r = 1/H.
deSitterExample :: Int -> Double -> HorizonResult
deSitterExample n hubble =
  let pts = lattice2D n
      cat = buildMeasCategory pts
      obs = deSitterHorizon hubble pts
      sub = accessibleSubcategory cat obs
      deficit = extensionDeficit cat sub
      kanLeft = kanExtensionLeft cat sub
      zeroOC = Map.size $ Map.filter (== 0) kanLeft
  in HorizonResult
    { hrObserverLabel      = hoLabel obs
    , hrTotalObjects       = Set.size (mcObjects cat)
    , hrAccessibleObjects  = Set.size (mcObjects sub)
    , hrTotalMorphisms     = Set.size (mcMorphisms cat)
    , hrAccessibleMorphisms = Set.size (mcMorphisms sub)
    , hrZeroOverCat        = zeroOC
    , hrDeficitMagnitude   = deficitMagnitude deficit
    , hrFaithful           = True
    , hrFull               = Set.size (mcObjects sub) == Set.size (mcObjects cat)
    }


-- ============================================================
-- Example: Cosmological Particle Horizon
-- ============================================================

-- | Cosmological particle horizon example.
-- Observer at (t_now, 0), horizon at comoving distance d_H.
cosmoExample :: Int -> Double -> HorizonResult
cosmoExample n dH =
  let -- Use a larger lattice for cosmological example
      pts = lattice2D n
      cat = buildMeasCategory pts
      tNow = fromIntegral (n `div` 2)
      obs = particleHorizon tNow dH pts
      sub = accessibleSubcategory cat obs
      deficit = extensionDeficit cat sub
      kanLeft = kanExtensionLeft cat sub
      zeroOC = Map.size $ Map.filter (== 0) kanLeft
  in HorizonResult
    { hrObserverLabel      = hoLabel obs
    , hrTotalObjects       = Set.size (mcObjects cat)
    , hrAccessibleObjects  = Set.size (mcObjects sub)
    , hrTotalMorphisms     = Set.size (mcMorphisms cat)
    , hrAccessibleMorphisms = Set.size (mcMorphisms sub)
    , hrZeroOverCat        = zeroOC
    , hrDeficitMagnitude   = deficitMagnitude deficit
    , hrFaithful           = True
    , hrFull               = Set.size (mcObjects sub) == Set.size (mcObjects cat)
    }


-- ============================================================
-- Example: Kan Extension Bracket
-- ============================================================

-- | Compute the Kan extension bracket (Lan, Ran) for a Rindler observer.
-- Demonstrates that the true description lies between Lan and Ran.
--
-- Note: This compares the structural density of constraints from the past
-- (over-categories for Lan) vs. the future (under-categories for Ran).
-- The bracket width |Right - Left| is a topological proxy for the rigorous
-- functorial bracket. In the enriched setting, the true bracket width
-- would be measured by norms on Banach-space-valued functors. Here we
-- use cardinalities as a finite, discrete approximation that captures the
-- qualitative feature: non-zero width implies observer ambiguity about
-- trans-horizon physics.
kanBracketExample :: Int -> BracketResult
kanBracketExample n =
  let pts = lattice2D n
      cat = buildMeasCategory pts
      obs = rindlerHorizon pts
      sub = accessibleSubcategory cat obs
      kanLeft  = kanExtensionLeft cat sub
      kanRight = kanExtensionRight cat sub
      leftTotal  = sum $ Map.elems kanLeft
      rightTotal = sum $ Map.elems kanRight
      -- Objects where left and right Kan extensions differ
      bracketObjs = Map.intersectionWith (\l r -> abs (l - r))
                      kanLeft kanRight
      objsInBracket = Map.size $ Map.filter (> 0) bracketObjs
  in BracketResult
    { brObserverLabel    = hoLabel obs
    , brLeftKanTotal     = leftTotal
    , brRightKanTotal    = rightTotal
    , brBracketWidth     = abs (rightTotal - leftTotal)
    , brObjectsInBracket = objsInBracket
    }

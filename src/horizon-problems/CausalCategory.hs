{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | CausalCategory.hs
-- Core categorical constructions for horizon problems.
-- Implements the causal measurement category, accessible subcategories,
-- Kan extensions, and extension deficit computation.
--
-- Reference: "Horizon Problems and the Yoneda Constraint" (Long, 2026)

module CausalCategory
  ( -- * Spacetime regions and causal structure
    SpacetimePoint(..)
  , CausalRegion(..)
  , CausalOrder(..)
  , causalPast
  , causalFuture
  , isCausallyConnected

    -- * Measurement category
  , MeasObject(..)
  , MeasMorphism(..)
  , MeasCategory(..)
  , composeMorphisms
  , identityMorphism

    -- * Accessible subcategory and horizons
  , HorizonType(..)
  , HorizonObserver(..)
  , accessibleSubcategory
  , isAccessible

    -- * Representable functor (Yoneda)
  , RepresentableFunctor(..)
  , yonedaEmbed
  , homSet

    -- * Kan extensions and deficit
  , OverCategory(..)
  , overCategory
  , kanExtensionLeft
  , kanExtensionRight
  , extensionDeficit
  , deficitMagnitude

    -- * Horizon-specific constructions
  , particleHorizon
  , eventHorizon
  , rindlerHorizon
  , deSitterHorizon
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, isJust)

-- ============================================================
-- Spacetime Points and Causal Structure
-- ============================================================

-- | A point in a discrete spacetime lattice.
-- Coordinates: (time, space_1, space_2, space_3)
data SpacetimePoint = STP
  { stTime   :: !Double
  , stSpace1 :: !Double
  , stSpace2 :: !Double
  , stSpace3 :: !Double
  } deriving (Eq, Ord, Show)

-- | A causally convex region in spacetime.
data CausalRegion = CausalRegion
  { regionId     :: !Int
  , regionPoints :: !(Set SpacetimePoint)
  , regionLabel  :: !String
  } deriving (Eq, Ord, Show)

-- | Causal ordering between spacetime points.
data CausalOrder
  = CausallyBefore    -- ^ p is in the causal past of q
  | CausallyAfter     -- ^ p is in the causal future of q
  | SpacelikeSeparated -- ^ p and q are spacelike separated
  | Coincident        -- ^ p = q
  deriving (Eq, Show)

-- | Determine causal relation between two points in Minkowski spacetime.
-- Uses the Minkowski metric: ds^2 = -c^2 dt^2 + dx^2 + dy^2 + dz^2
causalRelation :: SpacetimePoint -> SpacetimePoint -> CausalOrder
causalRelation p q
  | p == q = Coincident
  | interval < 0 && dt > 0 = CausallyBefore   -- p is in causal past of q
  | interval < 0 && dt < 0 = CausallyAfter     -- p is in causal future of q
  | interval == 0 = if dt > 0 then CausallyBefore else CausallyAfter  -- null
  | otherwise = SpacelikeSeparated
  where
    dt = stTime q - stTime p
    dx = stSpace1 q - stSpace1 p
    dy = stSpace2 q - stSpace2 p
    dz = stSpace3 q - stSpace3 p
    interval = -dt*dt + dx*dx + dy*dy + dz*dz  -- Minkowski interval (c=1)

-- | Compute the causal past J^-(p) within a set of points.
causalPast :: SpacetimePoint -> Set SpacetimePoint -> Set SpacetimePoint
causalPast p pts = Set.filter (\q -> causalRelation q p == CausallyBefore
                                  || q == p) pts

-- | Compute the causal future J^+(p) within a set of points.
causalFuture :: SpacetimePoint -> Set SpacetimePoint -> Set SpacetimePoint
causalFuture p pts = Set.filter (\q -> causalRelation p q == CausallyBefore
                                    || q == p) pts

-- | Check if two regions are causally connected.
isCausallyConnected :: CausalRegion -> CausalRegion -> Bool
isCausallyConnected r1 r2 = any (\p -> any (\q ->
    let rel = causalRelation p q
    in rel == CausallyBefore || rel == CausallyAfter
  ) (Set.toList $ regionPoints r2)) (Set.toList $ regionPoints r1)


-- ============================================================
-- Measurement Category
-- ============================================================

-- | An object in the causal measurement category.
-- Consists of a region, a state label (abstracting over the actual quantum state),
-- and an optional observer worldline.
data MeasObject = MeasObject
  { moRegion   :: !CausalRegion
  , moState    :: !String       -- ^ State label (e.g., "vacuum", "thermal_T=0.1")
  , moObserver :: !(Maybe [SpacetimePoint])  -- ^ Optional observer worldline
  } deriving (Eq, Ord, Show)

-- | A morphism in the causal measurement category.
-- Represents a causal embedding that preserves states.
data MeasMorphism = MeasMorphism
  { mmSource :: !MeasObject
  , mmTarget :: !MeasObject
  , mmLabel  :: !String   -- ^ Description of the embedding
  } deriving (Eq, Ord, Show)

-- | The full measurement category.
data MeasCategory = MeasCategory
  { mcObjects   :: !(Set MeasObject)
  , mcMorphisms :: !(Set MeasMorphism)
  } deriving (Show)

-- | Compose two morphisms if the target of the first matches the source of the second.
composeMorphisms :: MeasMorphism -> MeasMorphism -> Maybe MeasMorphism
composeMorphisms f g
  | mmTarget f == mmSource g = Just MeasMorphism
      { mmSource = mmSource f
      , mmTarget = mmTarget g
      , mmLabel  = mmLabel f ++ " ; " ++ mmLabel g
      }
  | otherwise = Nothing

-- | Identity morphism on an object.
identityMorphism :: MeasObject -> MeasMorphism
identityMorphism obj = MeasMorphism
  { mmSource = obj
  , mmTarget = obj
  , mmLabel  = "id"
  }


-- ============================================================
-- Accessible Subcategory and Horizons
-- ============================================================

-- | Types of physical horizons.
data HorizonType
  = ParticleHorizon     -- ^ Cosmological particle horizon
  | EventHorizon        -- ^ Black hole event horizon
  | RindlerHorizon      -- ^ Acceleration horizon
  | DeSitterHorizon     -- ^ Cosmological event horizon (Lambda > 0)
  | HolographicBound    -- ^ Bekenstein-Hawking area bound
  deriving (Eq, Show)

-- | An observer with a definite horizon.
data HorizonObserver = HorizonObserver
  { hoWorldline  :: ![SpacetimePoint]    -- ^ Observer's worldline
  , hoHorizon    :: !HorizonType          -- ^ Type of horizon
  , hoCausalPast :: !(Set SpacetimePoint) -- ^ J^-(gamma)
  , hoLabel      :: !String
  } deriving (Show)

-- | Compute the accessible subcategory for a horizon-bounded observer.
-- This is the full subcategory of MeasCategory on objects whose
-- regions are entirely within the observer's causal past.
accessibleSubcategory :: MeasCategory -> HorizonObserver -> MeasCategory
accessibleSubcategory cat obs = MeasCategory
  { mcObjects   = accessibleObjs
  , mcMorphisms = accessibleMorphs
  }
  where
    accessibleObjs = Set.filter (isAccessible obs) (mcObjects cat)
    accessibleMorphs = Set.filter (\m ->
        Set.member (mmSource m) accessibleObjs &&
        Set.member (mmTarget m) accessibleObjs
      ) (mcMorphisms cat)

-- | Check if a measurement object is accessible to the observer.
isAccessible :: HorizonObserver -> MeasObject -> Bool
isAccessible obs obj =
  Set.isSubsetOf (regionPoints $ moRegion obj) (hoCausalPast obs)


-- ============================================================
-- Representable Functor (Yoneda Embedding)
-- ============================================================

-- | The representable functor Hom(A, -) for an object A.
-- Represented as a map from objects to the set of morphisms from A.
data RepresentableFunctor = RepresentableFunctor
  { rfSource    :: !MeasObject
  , rfHomSets   :: !(Map MeasObject (Set MeasMorphism))
  } deriving (Show)

-- | Compute the Yoneda embedding of an object: the representable functor Hom(A, -).
yonedaEmbed :: MeasCategory -> MeasObject -> RepresentableFunctor
yonedaEmbed cat obj = RepresentableFunctor
  { rfSource  = obj
  , rfHomSets = Map.fromListWith Set.union
      [ (mmTarget m, Set.singleton m)
      | m <- Set.toList (mcMorphisms cat)
      , mmSource m == obj
      ]
  }

-- | Compute the hom-set Hom(A, B) in a measurement category.
homSet :: MeasCategory -> MeasObject -> MeasObject -> Set MeasMorphism
homSet cat a b = Set.filter (\m -> mmSource m == a && mmTarget m == b) (mcMorphisms cat)


-- ============================================================
-- Kan Extensions and Extension Deficit
-- ============================================================

-- | An object in the over-category (J \downarrow X).
-- Consists of an object c in the subcategory and a morphism J(c) -> X.
data OverCategory = OverCategory
  { ocSubObj  :: !MeasObject     -- ^ Object c in the accessible subcategory
  , ocMorphism :: !MeasMorphism  -- ^ Morphism J(c) -> X in the ambient category
  } deriving (Show)

-- | Compute the over-category (J \downarrow X) for the inclusion J and target X.
overCategory :: MeasCategory   -- ^ Ambient category
             -> MeasCategory   -- ^ Accessible subcategory
             -> MeasObject     -- ^ Target object X
             -> [OverCategory]
overCategory ambient sub target =
  [ OverCategory c m
  | c <- Set.toList (mcObjects sub)
  , m <- Set.toList (mcMorphisms ambient)
  , mmSource m == c
  , mmTarget m == target
  ]

-- | Compute the pointwise left Kan extension.
-- Returns the "size" of the colimit for each object in the ambient category.
-- A non-empty over-category means data is available; empty means no information.
kanExtensionLeft :: MeasCategory -> MeasCategory -> Map MeasObject Int
kanExtensionLeft ambient sub = Map.fromList
  [ (x, length $ overCategory ambient sub x)
  | x <- Set.toList (mcObjects ambient)
  ]

-- | Compute the pointwise right Kan extension (limit-based).
-- Returns the "size" of the limit for each object.
kanExtensionRight :: MeasCategory -> MeasCategory -> Map MeasObject Int
kanExtensionRight ambient sub = Map.fromList
  [ (x, length underCat)
  | x <- Set.toList (mcObjects ambient)
  , let underCat = [ (c, m)
                   | c <- Set.toList (mcObjects sub)
                   , m <- Set.toList (mcMorphisms ambient)
                   , mmSource m == x
                   , mmTarget m == c
                   ]
  ]

-- | Compute the extension deficit for each object.
-- The deficit is non-zero for objects outside the accessible subcategory
-- where the over-category is empty or thin.
extensionDeficit :: MeasCategory -> MeasCategory -> Map MeasObject Double
extensionDeficit ambient sub = Map.mapWithKey computeDeficit kanLeft
  where
    kanLeft = kanExtensionLeft ambient sub
    totalMorphismCount = Set.size (mcMorphisms ambient)

    computeDeficit obj overCatSize
      | Set.member obj (mcObjects sub) = 0.0  -- No deficit for accessible objects
      | overCatSize == 0 = 1.0                -- Maximal deficit: no information
      | otherwise = 1.0 - (fromIntegral overCatSize
                           / fromIntegral (max 1 totalMorphismCount))

-- | Compute the total deficit magnitude (scalar summary).
deficitMagnitude :: Map MeasObject Double -> Double
deficitMagnitude = sum . Map.elems


-- ============================================================
-- Horizon-Specific Constructions
-- ============================================================

-- | Construct a particle horizon observer in an FRW-like discrete spacetime.
-- The horizon is at comoving distance d_H from the observer at time t_now.
particleHorizon :: Double  -- ^ Current time t_now
                -> Double  -- ^ Horizon comoving distance d_H
                -> Set SpacetimePoint  -- ^ All spacetime points
                -> HorizonObserver
particleHorizon tNow dH allPts = HorizonObserver
  { hoWorldline  = [STP tNow 0 0 0]
  , hoHorizon    = ParticleHorizon
  , hoCausalPast = Set.filter withinHorizon allPts
  , hoLabel      = "Particle horizon observer at t=" ++ show tNow
  }
  where
    withinHorizon p =
      let dt = tNow - stTime p
          dr = sqrt (stSpace1 p ^ (2::Int) + stSpace2 p ^ (2::Int)
                   + stSpace3 p ^ (2::Int))
      in dt >= 0 && dr <= dH

-- | Construct an event horizon observer (Schwarzschild-like).
-- The horizon is at radius r_H from the origin.
eventHorizon :: Double  -- ^ Horizon radius r_H (Schwarzschild radius)
             -> Set SpacetimePoint  -- ^ All spacetime points
             -> HorizonObserver
eventHorizon rH allPts = HorizonObserver
  { hoWorldline  = [STP 0 (2*rH) 0 0]  -- Observer at 2*r_H
  , hoHorizon    = EventHorizon
  , hoCausalPast = Set.filter outsideHorizon allPts
  , hoLabel      = "Event horizon observer, r_H=" ++ show rH
  }
  where
    outsideHorizon p =
      let r = sqrt (stSpace1 p ^ (2::Int) + stSpace2 p ^ (2::Int)
                  + stSpace3 p ^ (2::Int))
      in r > rH

-- | Construct a Rindler horizon observer.
-- The right Rindler wedge is x > |t|.
rindlerHorizon :: Set SpacetimePoint -> HorizonObserver
rindlerHorizon allPts = HorizonObserver
  { hoWorldline  = [STP 0 1 0 0]  -- Observer at x=1
  , hoHorizon    = RindlerHorizon
  , hoCausalPast = Set.filter inRindlerWedge allPts
  , hoLabel      = "Rindler observer (right wedge)"
  }
  where
    inRindlerWedge p = stSpace1 p > abs (stTime p)

-- | Construct a de Sitter horizon observer.
-- The static patch is r < r_H = c/H.
deSitterHorizon :: Double  -- ^ Hubble parameter H (gives r_H = 1/H in c=1 units)
                -> Set SpacetimePoint
                -> HorizonObserver
deSitterHorizon hubble allPts = HorizonObserver
  { hoWorldline  = [STP 0 0 0 0]
  , hoHorizon    = DeSitterHorizon
  , hoCausalPast = Set.filter inStaticPatch allPts
  , hoLabel      = "de Sitter observer, H=" ++ show hubble
  }
  where
    rH = 1.0 / hubble
    inStaticPatch p =
      let r = sqrt (stSpace1 p ^ (2::Int) + stSpace2 p ^ (2::Int)
                  + stSpace3 p ^ (2::Int))
      in r < rH

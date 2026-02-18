{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | MeasurementCategory.hs
--
-- A type-level representation of the black hole measurement category
-- Meas_BH, as defined in Definition 3.1 of the paper.
--
-- Objects are triples (S, Sigma_S, rho_S) where S is an observer
-- subsystem, Sigma_S its causal domain, and rho_S the restricted
-- quantum state. Morphisms are CPTP maps satisfying state-preservation
-- and causal compatibility.
--
-- Reference: "The Black Hole Information Paradox from the Yoneda
-- Constraint Perspective" (Long, 2026), Section 3.

module MeasurementCategory
  ( -- * Spacetime regions
    Region(..)
  , RegionType(..)
    -- * Observer objects in Meas_BH
  , Observer(..)
  , mkAsymptoticObserver
  , mkInfallingObserver
  , mkRadiationObserver
  , mkInteriorObserver
    -- * Morphisms (CPTP channels)
  , Morphism(..)
  , composeMorphisms
  , identityMorphism
    -- * Causal structure
  , causallyAccessible
  , horizonObstruction
    -- * The measurement category
  , MeasCategory(..)
  , blackHoleMeasCategory
  , homSet
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Classification of spacetime regions relative to the black hole.
-- Corresponds to the causal structure in Definition 3.1.
data RegionType
  = Exterior      -- ^ Outside the event horizon (asymptotic region)
  | Horizon       -- ^ On the event horizon H^+
  | Interior      -- ^ Inside the event horizon
  | RadiationZone -- ^ Region of collected Hawking radiation
  deriving (Eq, Ord, Show)

-- | A spacetime region with an identifier and classification.
-- Represents Sigma_S, the causal domain of an observer.
data Region = Region
  { regionId   :: String
  , regionType :: RegionType
  , retardedTime :: Double
    -- ^ Retarded time u parameterizing the time-dependent
    -- subcategory Meas_BH(u) (Definition 3.5).
  } deriving (Eq, Ord, Show)

-- | An observer object in the black hole measurement category Meas_BH.
-- Corresponds to the triple (S, Sigma_S, rho_S) in Definition 3.1.
data Observer = Observer
  { observerId    :: String
  , causalDomain  :: Set Region
    -- ^ Sigma_S: the set of spacetime events causally accessible
    -- to this observer.
  , stateEntropy  :: Double
    -- ^ Von Neumann entropy S(rho_S) of the restricted state.
    -- Used as a proxy for the quantum state rho_S.
  , observerType  :: RegionType
    -- ^ Primary region type of this observer.
  } deriving (Eq, Ord, Show)

-- | The asymptotic observer S_infty (Definition 3.2).
-- Causal domain is the exterior of the black hole
-- (causal past of future null infinity I^+).
mkAsymptoticObserver :: Double -> Observer
mkAsymptoticObserver entropy = Observer
  { observerId   = "S_infty"
  , causalDomain = Set.fromList
      [ Region "exterior" Exterior 0
      , Region "radiation_zone" RadiationZone 0
      ]
  , stateEntropy = entropy
  , observerType = Exterior
  }

-- | The infalling observer S_in (Definition 3.3).
-- Causal domain crosses the horizon into the interior.
mkInfallingObserver :: Double -> Observer
mkInfallingObserver entropy = Observer
  { observerId   = "S_in"
  , causalDomain = Set.fromList
      [ Region "exterior" Exterior 0
      , Region "horizon" Horizon 0
      , Region "interior" Interior 0
      ]
  , stateEntropy = entropy
  , observerType = Interior
  }

-- | The early radiation observer S_Rad(u) at retarded time u
-- (Definition 3.4). Causal domain is the radiation collected
-- up to time u.
mkRadiationObserver :: Double -> Double -> Observer
mkRadiationObserver u entropy = Observer
  { observerId   = "S_Rad(" ++ show u ++ ")"
  , causalDomain = Set.fromList
      [ Region "radiation_zone" RadiationZone u
      , Region "exterior" Exterior u
      ]
  , stateEntropy = entropy
  , observerType = RadiationZone
  }

-- | An interior observer localized within the black hole.
mkInteriorObserver :: String -> Double -> Observer
mkInteriorObserver name entropy = Observer
  { observerId   = name
  , causalDomain = Set.singleton (Region "interior" Interior 0)
  , stateEntropy = entropy
  , observerType = Interior
  }

-- | A morphism in Meas_BH: a state-preserving CPTP channel
-- compatible with causal structure (Definition 3.1, item ii).
data Morphism = Morphism
  { morphismId :: String
  , source     :: Observer
  , target     :: Observer
  , channelFidelity :: Double
    -- ^ Fidelity of the CPTP channel (1.0 = perfect).
  } deriving (Eq, Show)

-- | Identity morphism for an observer (Definition 3.1, item iv).
identityMorphism :: Observer -> Morphism
identityMorphism obs = Morphism
  { morphismId     = "id_" ++ observerId obs
  , source         = obs
  , target         = obs
  , channelFidelity = 1.0
  }

-- | Composition of morphisms (Definition 3.1, item iii).
-- Sequential composition of CPTP maps, subject to causality.
composeMorphisms :: Morphism -> Morphism -> Maybe Morphism
composeMorphisms f g
  | observerId (target f) == observerId (source g) =
      Just Morphism
        { morphismId     = morphismId f ++ " ; " ++ morphismId g
        , source         = source f
        , target         = target g
        , channelFidelity = channelFidelity f * channelFidelity g
        }
  | otherwise = Nothing

-- | Check causal accessibility between two observers.
-- Region A is causally accessible from region B if A is in the
-- causal past of B. The horizon creates a one-way membrane.
causallyAccessible :: Observer -> Observer -> Bool
causallyAccessible src tgt =
  let srcType = observerType src
      tgtType = observerType tgt
  in case (srcType, tgtType) of
    -- Exterior -> Interior: blocked by horizon (Proposition 3.1)
    (Exterior, Interior)      -> False
    (RadiationZone, Interior) -> False
    -- Interior -> Exterior: also blocked (no signals escape)
    (Interior, Exterior)      -> False
    (Interior, RadiationZone) -> False
    -- Same region or compatible directions: allowed
    _                         -> True

-- | The horizon obstruction (Proposition 3.1):
-- Hom(S_infty, S_int) = empty for any interior object S_int.
horizonObstruction :: Observer -> Observer -> Bool
horizonObstruction src tgt =
  observerType src == Exterior && observerType tgt == Interior

-- | The black hole measurement category.
data MeasCategory = MeasCategory
  { objects   :: Set Observer
  , morphisms :: [Morphism]
  } deriving (Show)

-- | Construct the black hole measurement category with the
-- distinguished objects (Section 3.2) and the morphisms
-- consistent with the causal structure.
blackHoleMeasCategory :: Double -> MeasCategory
blackHoleMeasCategory entropy =
  let sInfty = mkAsymptoticObserver entropy
      sIn    = mkInfallingObserver entropy
      sRad   = mkRadiationObserver 0 entropy
      sInt   = mkInteriorObserver "S_int" entropy

      objs = Set.fromList [sInfty, sIn, sRad, sInt]

      -- Generate morphisms consistent with causal structure
      morps = [ Morphism "ext_to_ext" sInfty sRad 1.0
              , Morphism "in_to_int" sIn sInt 1.0
              , Morphism "in_to_ext" sIn sInfty 0.8
              , Morphism "rad_to_ext" sRad sInfty 1.0
              ]
              ++ map identityMorphism (Set.toList objs)

  in MeasCategory objs morps

-- | Compute the hom-set Hom(A, B) in the measurement category.
-- Returns the set of morphisms from A to B, respecting the
-- horizon obstruction (Proposition 3.1).
homSet :: MeasCategory -> Observer -> Observer -> [Morphism]
homSet cat src tgt
  | horizonObstruction src tgt = []  -- Proposition 3.1
  | otherwise = filter (\m -> observerId (source m) == observerId src
                            && observerId (target m) == observerId tgt)
                       (morphisms cat)

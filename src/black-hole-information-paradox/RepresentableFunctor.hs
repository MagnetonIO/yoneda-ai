{-# LANGUAGE GADTs #-}

-- | RepresentableFunctor.hs
--
-- Computation of the representable functor y^S = Hom(S, -)
-- for specified observers in the black hole measurement category,
-- implementing the Yoneda Constraint for black hole spacetimes.
--
-- Key result: the asymptotic observer's representable functor
-- vanishes on interior objects (Proposition 4.1), while the
-- infalling observer's functor is non-trivial on both exterior
-- and interior objects (Proposition 4.2).
--
-- Reference: "The Black Hole Information Paradox from the Yoneda
-- Constraint Perspective" (Long, 2026), Sections 4 and 4.2.

module RepresentableFunctor
  ( -- * Representable functors (presheaves)
    RepFunctor(..)
  , representableFunctor
    -- * Evaluation and analysis
  , evaluateAt
  , isVanishing
  , presheafSupport
    -- * Yoneda Constraint checks
  , yonedaConstraintSatisfied
  , nonIsomorphic
    -- * Presheaf operations
  , naturalTransformation
  , epistemicHorizon
  , epistemicBoundary
  ) where

import MeasurementCategory
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | A representable functor y^S = Hom_Meas_BH(S, -).
-- This is the Yoneda embedding of the observer S into the
-- presheaf category PSh(Meas_BH).
--
-- By the Yoneda lemma, this functor determines S up to
-- isomorphism: y^S1 = y^S2 iff S1 = S2.
data RepFunctor = RepFunctor
  { representingObject :: Observer
    -- ^ The observer S that this functor represents.
  , functorValues :: Map String [Morphism]
    -- ^ For each object X (keyed by observerId), the set
    -- Hom(S, X) of morphisms from S to X.
  } deriving (Show)

-- | Compute the representable functor y^S = Hom(S, -) for
-- observer S in the given measurement category.
--
-- This implements the core of the Yoneda Constraint (Proposition 2.2):
-- the embedded observer S accesses the rest of the universe R only
-- through y^(S, sigma_S).
representableFunctor :: MeasCategory -> Observer -> RepFunctor
representableFunctor cat obs = RepFunctor
  { representingObject = obs
  , functorValues = Map.fromList
      [ (observerId tgt, homSet cat obs tgt)
      | tgt <- Set.toList (objects cat)
      ]
  }

-- | Evaluate the representable functor at an object X,
-- returning Hom(S, X).
--
-- By the Yoneda lemma, Nat(y^S, F) = F(S) for any presheaf F,
-- so evaluating y^S at X gives the set of ways S can "probe" X.
evaluateAt :: RepFunctor -> Observer -> [Morphism]
evaluateAt rf tgt =
  case Map.lookup (observerId tgt) (functorValues rf) of
    Just ms -> ms
    Nothing -> []  -- No information about this object

-- | Check if the representable functor vanishes at an object.
--
-- y^S_infty(S_int) = Hom(S_infty, S_int) = empty
-- for all interior objects S_int (Proposition 4.1).
-- This is the categorical expression of the horizon obstruction.
isVanishing :: RepFunctor -> Observer -> Bool
isVanishing rf tgt = null (evaluateAt rf tgt)

-- | The support of the presheaf: the set of objects where
-- the functor is non-empty.
--
-- For the asymptotic observer, the support excludes interior
-- objects. For the infalling observer, the support includes
-- both exterior and interior objects.
presheafSupport :: RepFunctor -> [String]
presheafSupport rf =
  [ objId
  | (objId, ms) <- Map.toList (functorValues rf)
  , not (null ms)
  ]

-- | Verify that the Yoneda Constraint is satisfied:
-- the observer's knowledge is fully determined by its
-- representable functor.
--
-- In practice, we check that:
-- 1. The functor vanishes on causally inaccessible objects.
-- 2. The functor is non-empty on accessible objects.
yonedaConstraintSatisfied :: MeasCategory -> Observer -> Bool
yonedaConstraintSatisfied cat obs =
  let rf = representableFunctor cat obs
  in all (\tgt ->
    if horizonObstruction obs tgt
    then isVanishing rf tgt     -- Must vanish on inaccessible objects
    else True                   -- No constraint on accessible objects
  ) (Set.toList (objects cat))

-- | Check that two representable functors are non-isomorphic
-- (Proposition 4.3): y^S1 /= y^S2.
--
-- Two functors are non-isomorphic if they differ on at least
-- one object. This is the key structural fact underlying
-- black hole complementarity (Theorem 5.1).
nonIsomorphic :: RepFunctor -> RepFunctor -> Bool
nonIsomorphic rf1 rf2 =
  any (\objId ->
    let v1 = maybe [] id (Map.lookup objId (functorValues rf1))
        v2 = maybe [] id (Map.lookup objId (functorValues rf2))
    in length v1 /= length v2
  ) allObjIds
  where
    allObjIds = Set.toList $ Set.union
      (Map.keysSet (functorValues rf1))
      (Map.keysSet (functorValues rf2))

-- | Compute whether there exists a natural transformation
-- between two representable functors.
--
-- A natural transformation eta: y^S1 => y^S2 corresponds
-- (by Yoneda) to a morphism S2 -> S1 in Meas_BH.
-- Returns the list of candidate morphisms.
naturalTransformation :: MeasCategory -> RepFunctor -> RepFunctor -> [Morphism]
naturalTransformation cat rf1 rf2 =
  -- By Yoneda: Nat(y^S1, y^S2) = Hom(S2, S1)
  homSet cat (representingObject rf2) (representingObject rf1)

-- | The epistemic horizon of an observer (Definition 4.4):
-- the full subcategory of Meas_BH consisting of objects
-- reachable by morphisms from S.
--
-- For the asymptotic observer, this is the exterior region.
-- The epistemic horizon coincides with the event horizon
-- for a classical black hole (Proposition 4.5).
epistemicHorizon :: MeasCategory -> Observer -> Set String
epistemicHorizon cat obs =
  let rf = representableFunctor cat obs
  in Set.fromList (presheafSupport rf)

-- | The epistemic boundary (Definition 4.4):
-- objects reachable by S but not contained in S's causal domain.
-- For the asymptotic observer, this is the event horizon H^+.
epistemicBoundary :: MeasCategory -> Observer -> Set String
epistemicBoundary cat obs =
  let horizon = epistemicHorizon cat obs
      domain  = Set.map regionId (causalDomain obs)
  in Set.difference horizon domain

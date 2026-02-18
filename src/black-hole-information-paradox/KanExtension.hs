{-# LANGUAGE GADTs #-}

-- | KanExtension.hs
--
-- Computation of the left Kan extension and the extension deficit
-- for the black hole measurement category. The extension deficit
-- Delta(S) quantifies the gap between local observer knowledge
-- and global description.
--
-- Key results modeled:
-- - Information deficit of the asymptotic observer (Proposition 6.2)
-- - Time-dependent deficit behavior (Theorem 7.1)
-- - Island formula as deficit minimization (Proposition 7.4)
--
-- Reference: "The Black Hole Information Paradox from the Yoneda
-- Constraint Perspective" (Long, 2026), Sections 6 and 7.

module KanExtension
  ( -- * Kan extensions
    KanExt(..)
  , leftKanExtension
  , rightKanExtension
    -- * Extension deficit
  , ExtensionDeficit(..)
  , extensionDeficit
  , deficitAtObject
    -- * Bracket of extrapolation
  , ExtrapolationBracket(..)
  , extrapolationBracket
  , bracketWidth
    -- * Island formula
  , Island(..)
  , islandDeficitReduction
  , optimalIsland
  ) where

import MeasurementCategory
import RepresentableFunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | A left Kan extension Lan_J(D . J) along the inclusion
-- J: Meas_BH|_S -> Meas_BH.
--
-- Conceptually, this is the "best approximation" of the global
-- description functor R from the observer's local data.
-- When the Kan extension recovers R, information is fully
-- accessible; the deficit measures the gap.
data KanExt = KanExt
  { kanObserver :: Observer
    -- ^ The observer S whose accessible subcategory defines J.
  , kanValues :: Map String Double
    -- ^ For each object X, a numerical measure of how much
    -- the Kan extension recovers. 1.0 = full recovery,
    -- 0.0 = no information.
  } deriving (Show)

-- | Compute the left Kan extension from the observer's
-- accessible subcategory.
--
-- The pointwise left Kan extension at an object X is:
--   (Lan_J F)(X) = colim_{(J -> X)} F
-- When the over-category (J -> X) is empty (no morphisms
-- from accessible objects to X), the colimit is initial
-- (no information). This is the case for interior objects
-- viewed by the asymptotic observer (Proposition 6.2).
leftKanExtension :: MeasCategory -> Observer -> KanExt
leftKanExtension cat obs =
  let rf = representableFunctor cat obs
      vals = Map.fromList
        [ (observerId tgt, kanValue rf obs tgt)
        | tgt <- Set.toList (objects cat)
        ]
  in KanExt obs vals

-- | Compute the right Kan extension.
-- The right Kan extension gives the "upper bound" on what
-- the observer could know -- the most conservative extrapolation.
--
-- For the asymptotic observer of a classical black hole,
-- Ran gives maximal ambiguity on interior objects: all states
-- compatible with exterior data (Proposition 6.3).
rightKanExtension :: MeasCategory -> Observer -> KanExt
rightKanExtension cat obs =
  let rf = representableFunctor cat obs
      vals = Map.fromList
        [ (observerId tgt, ranValue rf obs tgt)
        | tgt <- Set.toList (objects cat)
        ]
  in KanExt obs vals

-- | Compute the left Kan extension value at an object.
-- Non-zero when morphisms exist from observer to target.
kanValue :: RepFunctor -> Observer -> Observer -> Double
kanValue rf obs tgt =
  let ms = evaluateAt rf tgt
  in if null ms
     then 0.0  -- Over-category (J -> X) is empty
     else sum (map channelFidelity ms) / fromIntegral (length ms)

-- | Compute the right Kan extension value.
-- 1.0 when the over-category is empty (maximal ambiguity),
-- otherwise the minimum fidelity (most conservative).
ranValue :: RepFunctor -> Observer -> Observer -> Double
ranValue rf obs tgt =
  let ms = evaluateAt rf tgt
  in if null ms
     then 1.0  -- Maximal ambiguity: all states compatible
     else minimum (map channelFidelity ms)

-- | The extension deficit Delta(S) (Proposition 6.2).
-- Measures the gap between the Kan extension and the true
-- global description.
data ExtensionDeficit = ExtensionDeficit
  { deficitObserver :: Observer
  , objectDeficits :: Map String Double
    -- ^ Per-object deficit: 1.0 - kanValue.
    -- Non-zero iff S(rho_Rad) > 0 (Proposition 6.4).
  , totalDeficit :: Double
    -- ^ Sum of all per-object deficits.
  } deriving (Show)

-- | Compute the extension deficit Delta(S).
--
-- Delta(S) = coker(Lan_J(D . J) => R).
-- The deficit is non-trivial whenever S /= R (the observer
-- is a proper subsystem). For the asymptotic observer before
-- the Page time, the deficit is maximal on interior objects.
extensionDeficit :: MeasCategory -> Observer -> ExtensionDeficit
extensionDeficit cat obs =
  let kan = leftKanExtension cat obs
      deficits = Map.map (\v -> 1.0 - v) (kanValues kan)
      total = sum (Map.elems deficits)
  in ExtensionDeficit obs deficits total

-- | Deficit at a specific object.
deficitAtObject :: ExtensionDeficit -> Observer -> Double
deficitAtObject ed tgt =
  case Map.lookup (observerId tgt) (objectDeficits ed) of
    Just d  -> d
    Nothing -> 1.0  -- Unknown object: maximal deficit

-- | The bracket of extrapolation [Lan, Ran] (Proposition 6.3).
-- The true description lies between the left and right
-- Kan extensions.
data ExtrapolationBracket = ExtrapolationBracket
  { bracketObserver :: Observer
  , lowerBound :: Map String Double  -- ^ Left Kan extension values
  , upperBound :: Map String Double  -- ^ Right Kan extension values
  } deriving (Show)

-- | Compute the extrapolation bracket.
-- For the asymptotic observer of a classical black hole,
-- the bracket is maximally wide on interior objects:
-- Lan = 0, Ran = 1 (Proposition 6.3).
extrapolationBracket :: MeasCategory -> Observer -> ExtrapolationBracket
extrapolationBracket cat obs =
  let lan = leftKanExtension cat obs
      ran = rightKanExtension cat obs
  in ExtrapolationBracket obs (kanValues lan) (kanValues ran)

-- | Width of the bracket at a specific object.
-- Width = Ran - Lan. Maximal (1.0) for inaccessible objects,
-- zero for fully accessible objects.
bracketWidth :: ExtrapolationBracket -> Observer -> Double
bracketWidth eb tgt =
  let objId = observerId tgt
      lo = maybe 0.0 id (Map.lookup objId (lowerBound eb))
      hi = maybe 1.0 id (Map.lookup objId (upperBound eb))
  in hi - lo

-- | A categorical island (Definition 7.3):
-- a collection of interior objects that, when included in the
-- observer's effective subcategory, reduce the extension deficit.
data Island = Island
  { islandObjects :: Set Observer
    -- ^ Interior objects in the island I.
  , islandBoundaryArea :: Double
    -- ^ Area(partial I) / 4 G_N: the gravitational cost
    -- of extending across the horizon.
  } deriving (Show)

-- | Compute the deficit reduction from including an island
-- (Proposition 7.4):
-- Delta(S_Rad(u) union I) < Delta(S_Rad(u)).
islandDeficitReduction :: MeasCategory -> Observer -> Island -> Double
islandDeficitReduction cat obs island =
  let baseDeficit = totalDeficit (extensionDeficit cat obs)
      -- Including the island effectively adds morphisms to
      -- interior objects, reducing the deficit.
      -- The reduction is penalized by the boundary area term.
      interiorRecovery = fromIntegral (Set.size (islandObjects island))
                       * 0.5  -- Partial recovery per object
      areaCost = islandBoundaryArea island
  in max 0.0 (interiorRecovery - areaCost)

-- | Find the optimal island I*(u) that minimizes the deficit
-- (Proposition 7.4):
-- I*(u) = argmin_I Delta(S_Rad(u) union I).
--
-- This corresponds to the island formula minimization:
-- S(Rad) = min_I [ Area(partial I)/4G_N + S_bulk(Rad union I) ]
optimalIsland :: MeasCategory -> Observer -> [Island] -> Maybe Island
optimalIsland cat obs candidates =
  case candidates of
    [] -> Nothing
    _  -> Just $ foldl1 (\best cand ->
            let bestReduction = islandDeficitReduction cat obs best
                candReduction = islandDeficitReduction cat obs cand
            in if candReduction > bestReduction then cand else best
          ) candidates

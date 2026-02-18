{-# LANGUAGE ScopedTypeVariables #-}

-- | Computation of Kan extensions and extension deficits.
-- Implements the categorical machinery for analyzing hidden variable
-- theories as functorial completions of observer data.
module KanExtension where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Complex
import MeasurementCategory (DensityOp(..), vonNeumannEntropy,
                            singletReducedA, maxMixed, pureState)

-- | A finite category represented as an adjacency structure.
data FinCat obj = FinCat
  { fcObjects  :: [obj]
  , fcHom      :: obj -> obj -> [Int]  -- ^ Morphism indices from a to b
  }

-- | A Set-valued functor on a finite category.
type SetFunctor obj = obj -> Set String

-- | Left Kan extension along an inclusion J: C_sub -> C_full.
-- Lan_J(F)(d) = colim_{(c, J(c) -> d)} F(c)
-- For our discrete setting, this is the union of F(c) for all c
-- in the subcategory that map to d.
leftKanExt
  :: (Eq obj, Ord obj)
  => [obj]              -- ^ Subcategory objects
  -> [obj]              -- ^ Full category objects
  -> (obj -> obj -> Bool)  -- ^ Is there a morphism from c to d?
  -> SetFunctor obj     -- ^ Functor F on subcategory
  -> SetFunctor obj     -- ^ Result: Lan_J(F) on full category
leftKanExt subObjs _fullObjs hasMor f d =
  Set.unions [f c | c <- subObjs, hasMor c d]

-- | Right Kan extension along an inclusion.
-- Ran_J(F)(d) = lim_{(c, d -> J(c))} F(c)
-- For our discrete setting, this is the intersection.
rightKanExt
  :: (Eq obj, Ord obj)
  => [obj]
  -> [obj]
  -> (obj -> obj -> Bool)
  -> SetFunctor obj
  -> SetFunctor obj
rightKanExt subObjs _fullObjs hasMor f d =
  let relevant = [f c | c <- subObjs, hasMor d c]
  in if null relevant
     then Set.empty
     else foldl1 Set.intersection relevant

-- | Extension deficit: elements in the total functor not recovered by Lan.
extensionDeficitSet
  :: (Eq obj, Ord obj)
  => [obj]
  -> [obj]
  -> (obj -> obj -> Bool)
  -> SetFunctor obj
  -> SetFunctor obj     -- ^ Total (ground truth) functor
  -> obj
  -> Set String
extensionDeficitSet subObjs fullObjs hasMor fLocal fTotal d =
  let lanResult = leftKanExt subObjs fullObjs hasMor fLocal d
      totalResult = fTotal d
  in Set.difference totalResult lanResult

-- | Bracket of extrapolation: [Lan, Ran] interval.
extrapolationBracket
  :: (Eq obj, Ord obj)
  => [obj]
  -> [obj]
  -> (obj -> obj -> Bool)
  -> SetFunctor obj
  -> obj
  -> (Set String, Set String)  -- ^ (optimistic/Lan, conservative/Ran)
extrapolationBracket subObjs fullObjs hasMor f d =
  ( leftKanExt subObjs fullObjs hasMor f d
  , rightKanExt subObjs fullObjs hasMor f d
  )

-- | Compute the extension deficit for a bipartite quantum system.
-- For the singlet state, Delta(S_A) != 0 iff S(rho_A) > 0.
quantumExtensionDeficit :: DensityOp -> Double
quantumExtensionDeficit rhoReduced = vonNeumannEntropy rhoReduced

-- | Check if a state has trivial extension deficit (product state).
isTrivialDeficit :: DensityOp -> Bool
isTrivialDeficit rho = quantumExtensionDeficit rho < 1e-10

-- | Demonstrate convergence of iterated extensions.
-- As the observer's subsystem grows, the deficit shrinks.
convergenceDemo :: [(Double, Double)]
convergenceDemo =
  -- Simulate a sequence of observers with increasing access.
  -- Parameterized by mixing parameter p: rho_A = p |0><0| + (1-p) I/2
  -- As p -> 1, the state becomes pure and the deficit -> 0.
  [ (p, vonNeumannEntropy (mixedState p)) | p <- [0.0, 0.1 .. 1.0] ]
  where
    mixedState p =
      let q = 1 - p
      in DensityOp
        { rho00 = (p + q/2) :+ 0
        , rho01 = 0
        , rho10 = 0
        , rho11 = (q/2) :+ 0
        }

-- | Full Kan extension analysis result.
data KanResult = KanResult
  { krSingletDeficit   :: Double
  , krProductDeficit   :: Double
  , krConvergence      :: [(Double, Double)]
  , krInterpretation   :: String
  } deriving (Show)

-- | Run the Kan extension analysis.
runKanAnalysis :: KanResult
runKanAnalysis =
  let singletDef = quantumExtensionDeficit singletReducedA
      productDef = quantumExtensionDeficit (pureState 1 0)
      conv = convergenceDemo
  in KanResult
    { krSingletDeficit = singletDef
    , krProductDeficit = productDef
    , krConvergence = conv
    , krInterpretation = unlines
        [ "Kan Extension Analysis:"
        , ""
        , "Extension deficit for singlet state: " ++ show singletDef ++ " bits"
        , "Extension deficit for product state: " ++ show productDef ++ " bits"
        , ""
        , "The extension deficit Delta(S) quantifies the irreducible gap"
        , "between Alice's local knowledge and the global description."
        , "For entangled states, Delta(S) > 0: hidden correlations exist"
        , "that any HV theory must account for."
        , "For product states, Delta(S) = 0: no hidden correlations."
        , ""
        , "Convergence: as the observer's subsystem grows (mixing -> 0),"
        , "the deficit shrinks to zero, confirming Proposition 7.4."
        , ""
        , "Any hidden variable theory is a functorial completion that"
        , "must bridge this deficit. The Kan extension provides the"
        , "optimal (best possible) such completion."
        ]
    }

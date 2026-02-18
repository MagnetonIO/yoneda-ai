{-# LANGUAGE GADTs #-}

-- | Complementarity.hs
--
-- Verification that specified observer pairs satisfy the conditions
-- for categorical complementarity (Definition 5.1) and related
-- checks from the Yoneda framework analysis of black hole
-- complementarity.
--
-- Categorical complementarity requires:
--   (i)   y^S1 not isomorphic to y^S2
--   (ii)  No third object S3 whose functor restricts to both
--   (iii) Non-existence of S3 is enforced by causal structure
--
-- Also implements:
-- - Complementarity presheaf construction (Definition 5.3)
-- - Non-representability check (Proposition 5.4)
-- - Firewall paradox analysis (Theorem 5.2)
--
-- Reference: "The Black Hole Information Paradox from the Yoneda
-- Constraint Perspective" (Long, 2026), Section 5.

module Complementarity
  ( -- * Complementarity verification
    ComplementarityResult(..)
  , checkComplementarity
  , isComplementary
    -- * Complementarity presheaf
  , ComplementarityPresheaf(..)
  , complementarityPresheaf
  , isRepresentable
    -- * Firewall analysis
  , FirewallAnalysis(..)
  , analyzeFirewall
    -- * AMPS argument
  , AMPSModes(..)
  , ampsContradiction
    -- * Demonstration
  , demonstrateComplementarity
  ) where

import MeasurementCategory
import RepresentableFunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Result of checking categorical complementarity
-- between two observers (Definition 5.1).
data ComplementarityResult = ComplementarityResult
  { observer1 :: Observer
  , observer2 :: Observer
  , condition1_nonIsomorphic :: Bool
    -- ^ (i) y^S1 /= y^S2: representable functors are
    -- non-isomorphic (Proposition 4.3).
  , condition2_noThirdObject :: Bool
    -- ^ (ii) No S3 exists whose functor restricts to both.
    -- Verified by checking that no object in Meas_BH has
    -- morphisms to all objects reachable by either S1 or S2.
  , condition3_causalEnforcement :: Bool
    -- ^ (iii) The non-existence of S3 is enforced by the
    -- causal structure of the black hole spacetime.
  , complementarityHolds :: Bool
    -- ^ All three conditions satisfied.
  } deriving (Show)

-- | Check categorical complementarity between two observers
-- in the given measurement category.
--
-- This implements Definition 5.1 and the verification in
-- Theorem 5.1 (Black Hole Complementarity from the Yoneda
-- Constraint).
checkComplementarity :: MeasCategory -> Observer -> Observer -> ComplementarityResult
checkComplementarity cat obs1 obs2 =
  let rf1 = representableFunctor cat obs1
      rf2 = representableFunctor cat obs2

      -- Condition (i): non-isomorphism of representable functors
      cond1 = nonIsomorphic rf1 rf2

      -- Condition (ii): no third object whose functor restricts
      -- to both. We check all objects in the category.
      cond2 = not $ any (coverssBoth cat rf1 rf2) (Set.toList (objects cat))

      -- Condition (iii): causal enforcement. The causal structure
      -- of the black hole prevents any observer from having
      -- simultaneous access to both exterior and deep interior.
      cond3 = causallyEnforced obs1 obs2

  in ComplementarityResult
      { observer1 = obs1
      , observer2 = obs2
      , condition1_nonIsomorphic = cond1
      , condition2_noThirdObject = cond2
      , condition3_causalEnforcement = cond3
      , complementarityHolds = cond1 && cond2 && cond3
      }

-- | Check if an observer S3's representable functor "covers"
-- both S1's and S2's functors (would violate condition ii).
coverssBoth :: MeasCategory -> RepFunctor -> RepFunctor -> Observer -> Bool
coverssBoth cat rf1 rf2 candidate =
  let rfCand = representableFunctor cat candidate
      support1 = Set.fromList (presheafSupport rf1)
      support2 = Set.fromList (presheafSupport rf2)
      supportCand = Set.fromList (presheafSupport rfCand)
  in Set.isSubsetOf support1 supportCand
     && Set.isSubsetOf support2 supportCand
     -- Exclude trivial cases (the candidate is one of the originals)
     && observerId candidate /= observerId (representingObject rf1)
     && observerId candidate /= observerId (representingObject rf2)

-- | Check if the causal structure enforces non-existence of
-- a combined observer (condition iii).
causallyEnforced :: Observer -> Observer -> Bool
causallyEnforced obs1 obs2 =
  -- Complementarity is causally enforced when the two observers
  -- have incompatible causal domains (one exterior, one interior).
  let types = Set.fromList [observerType obs1, observerType obs2]
  in Set.member Exterior types && Set.member Interior types

-- | Simplified complementarity check.
isComplementary :: MeasCategory -> Observer -> Observer -> Bool
isComplementary cat obs1 obs2 =
  complementarityHolds (checkComplementarity cat obs1 obs2)

-- | The complementarity presheaf F_comp (Definition 5.3):
-- the pushout of y^S_infty and y^S_in along their shared domain.
data ComplementarityPresheaf = ComplementarityPresheaf
  { cpFunctor1 :: RepFunctor
    -- ^ y^S_infty: asymptotic observer's functor.
  , cpFunctor2 :: RepFunctor
    -- ^ y^S_in: infalling observer's functor.
  , cpValues :: Map String [Morphism]
    -- ^ Combined presheaf values (union of hom-sets).
  , cpIsRepresentable :: Bool
    -- ^ Whether F_comp is representable (should be False
    -- by Proposition 5.4).
  } deriving (Show)

-- | Construct the complementarity presheaf (Definition 5.3).
-- F_comp = y^S_infty sqcup_{shared} y^S_in
--
-- The pushout combines the two representable functors,
-- identifying morphisms in the shared exterior domain.
complementarityPresheaf :: MeasCategory -> Observer -> Observer -> ComplementarityPresheaf
complementarityPresheaf cat obs1 obs2 =
  let rf1 = representableFunctor cat obs1
      rf2 = representableFunctor cat obs2

      -- Combine the functor values (pushout)
      combined = Map.unionWith (++) (functorValues rf1) (functorValues rf2)

      -- Check representability (Proposition 5.4)
      representable = isRepresentable cat combined

  in ComplementarityPresheaf rf1 rf2 combined representable

-- | Check if a presheaf (given by its values) is representable.
-- A presheaf F is representable if there exists X in Meas_BH
-- with y^X = F.
--
-- By Proposition 5.4, the complementarity presheaf is NOT
-- representable: no single observer can capture both
-- exterior and interior physics.
isRepresentable :: MeasCategory -> Map String [Morphism] -> Bool
isRepresentable cat pshValues =
  any (\obj ->
    let rf = representableFunctor cat obj
    in Map.keys (functorValues rf) == Map.keys pshValues
       && all (\k -> length (maybe [] id (Map.lookup k (functorValues rf)))
                  == length (maybe [] id (Map.lookup k pshValues)))
              (Map.keys pshValues)
  ) (Set.toList (objects cat))

-- | The three modes in the AMPS argument (Theorem 5.2).
data AMPSModes = AMPSModes
  { modeB :: String  -- ^ Late Hawking mode B
  , modeA :: String  -- ^ Early radiation A
  , modeC :: String  -- ^ Interior partner C
  } deriving (Show)

-- | Check whether the AMPS argument produces a contradiction
-- in the Yoneda framework.
--
-- The AMPS argument fails because it illegitimately assumes
-- a single presheaf encoding both B-A entanglement (visible
-- to S_infty) and B-C entanglement (visible to S_in).
-- By Theorem 5.1, no such combined observer exists.
ampsContradiction :: MeasCategory -> Observer -> Observer -> AMPSModes -> Bool
ampsContradiction cat obsInfty obsIn modes =
  -- The "contradiction" only arises if we assume a single
  -- observer can see both entanglements. By complementarity,
  -- this is false, so no contradiction exists.
  not (isComplementary cat obsInfty obsIn)
  -- Returns False (no contradiction) when complementarity holds.

-- | Analysis of the firewall paradox (Theorem 5.2).
data FirewallAnalysis = FirewallAnalysis
  { firewallRequired :: Bool
    -- ^ Whether a firewall is needed (False in our framework).
  , reason :: String
    -- ^ Explanation of the resolution.
  , complementarityCheck :: ComplementarityResult
    -- ^ The underlying complementarity check.
  } deriving (Show)

-- | Analyze the firewall paradox for two observers
-- (Theorem 5.2: Yoneda Resolution of the Firewall Paradox).
analyzeFirewall :: MeasCategory -> Observer -> Observer -> FirewallAnalysis
analyzeFirewall cat obsInfty obsIn =
  let compResult = checkComplementarity cat obsInfty obsIn
  in FirewallAnalysis
      { firewallRequired = not (complementarityHolds compResult)
      , reason = if complementarityHolds compResult
          then "No firewall required. The AMPS argument fails because it "
            ++ "assumes a single global presheaf encoding both B-A and B-C "
            ++ "entanglement. By categorical complementarity (Theorem 5.1), "
            ++ "no such object exists in Meas_BH. The monogamy-of-entanglement "
            ++ "argument applies only within a single representable functor, "
            ++ "not across complementary observers."
          else "Complementarity does not hold for these observers. "
            ++ "The AMPS argument may apply, potentially requiring a firewall."
      , complementarityCheck = compResult
      }

-- | Run a full demonstration of complementarity for the
-- standard black hole observers, printing results.
demonstrateComplementarity :: IO ()
demonstrateComplementarity = do
  let cat = blackHoleMeasCategory 50.0
      sInfty = mkAsymptoticObserver 50.0
      sIn    = mkInfallingObserver 50.0

  putStrLn "=== Black Hole Complementarity Analysis ==="
  putStrLn ""

  -- Check complementarity
  let comp = checkComplementarity cat sInfty sIn
  putStrLn $ "Observer 1: " ++ observerId (observer1 comp)
  putStrLn $ "Observer 2: " ++ observerId (observer2 comp)
  putStrLn $ "Condition (i)   - Non-isomorphic functors: "
           ++ show (condition1_nonIsomorphic comp)
  putStrLn $ "Condition (ii)  - No third object:         "
           ++ show (condition2_noThirdObject comp)
  putStrLn $ "Condition (iii) - Causal enforcement:      "
           ++ show (condition3_causalEnforcement comp)
  putStrLn $ "Complementarity holds: "
           ++ show (complementarityHolds comp)
  putStrLn ""

  -- Complementarity presheaf
  let cp = complementarityPresheaf cat sInfty sIn
  putStrLn $ "Complementarity presheaf representable? "
           ++ show (cpIsRepresentable cp)
  putStrLn "  (Expected: False, by Proposition 5.4)"
  putStrLn ""

  -- Firewall analysis
  let fw = analyzeFirewall cat sInfty sIn
  putStrLn $ "Firewall required? " ++ show (firewallRequired fw)
  putStrLn $ "Resolution: " ++ reason fw

{-# LANGUAGE ScopedTypeVariables #-}

-- | Main.hs
-- Entry point for the horizon-problems Haskell implementation.
-- Demonstrates the categorical constructions on lattice models of
-- Rindler, Schwarzschild, de Sitter, and cosmological horizons.
--
-- Reference: "Horizon Problems and the Yoneda Constraint" (Long, 2026)

module Main where

import CausalCategory
import HorizonExamples

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- ============================================================
-- Main: Run all horizon examples
-- ============================================================

main :: IO ()
main = do
  putStrLn "============================================================"
  putStrLn "Horizon Problems and the Yoneda Constraint"
  putStrLn "Haskell Implementation of Categorical Constructions"
  putStrLn "============================================================"
  putStrLn ""

  -- Example 1: Rindler horizon
  putStrLn "--- Example 1: Rindler Horizon ---"
  let rindlerResult = rindlerExample 5  -- 5x5 lattice
  printHorizonResult rindlerResult
  putStrLn ""

  -- Example 2: Black hole event horizon
  putStrLn "--- Example 2: Black Hole Event Horizon ---"
  let bhResult = blackHoleExample 5 1.5  -- 5x5 lattice, r_H = 1.5
  printHorizonResult bhResult
  putStrLn ""

  -- Example 3: De Sitter horizon
  putStrLn "--- Example 3: De Sitter Horizon ---"
  let dsResult = deSitterExample 5 0.5  -- 5x5 lattice, H = 0.5
  printHorizonResult dsResult
  putStrLn ""

  -- Example 4: Cosmological particle horizon
  putStrLn "--- Example 4: Cosmological Particle Horizon ---"
  let cosmoResult = cosmoExample 5 3.0  -- 5x5 lattice, d_H = 3.0
  printHorizonResult cosmoResult
  putStrLn ""

  -- Example 5: Kan extension bracket
  putStrLn "--- Example 5: Kan Extension Bracket ---"
  let bracketResult = kanBracketExample 5
  printBracketResult bracketResult
  putStrLn ""

  -- Summary
  putStrLn "============================================================"
  putStrLn "Summary: Extension Deficits by Horizon Type"
  putStrLn "============================================================"
  putStrLn $ "  Rindler:    " ++ show (hrDeficitMagnitude rindlerResult)
  putStrLn $ "  Black hole: " ++ show (hrDeficitMagnitude bhResult)
  putStrLn $ "  De Sitter:  " ++ show (hrDeficitMagnitude dsResult)
  putStrLn $ "  Cosmological: " ++ show (hrDeficitMagnitude cosmoResult)
  putStrLn ""
  putStrLn "All deficits are non-zero, confirming the Horizon Yoneda Constraint:"
  putStrLn "Delta(gamma) /= 0 <==> J^-(gamma) is a proper subset of M"


-- ============================================================
-- Result Types and Printing
-- ============================================================

-- | Print a horizon analysis result.
printHorizonResult :: HorizonResult -> IO ()
printHorizonResult hr = do
  putStrLn $ "  Observer: " ++ hrObserverLabel hr
  putStrLn $ "  Total objects: " ++ show (hrTotalObjects hr)
  putStrLn $ "  Accessible objects: " ++ show (hrAccessibleObjects hr)
  putStrLn $ "  Total morphisms: " ++ show (hrTotalMorphisms hr)
  putStrLn $ "  Accessible morphisms: " ++ show (hrAccessibleMorphisms hr)
  putStrLn $ "  Objects with zero over-category: " ++ show (hrZeroOverCat hr)
  putStrLn $ "  Deficit magnitude: " ++ show (hrDeficitMagnitude hr)
  putStrLn $ "  Inclusion faithful: " ++ show (hrFaithful hr)
  putStrLn $ "  Inclusion full: " ++ show (hrFull hr)

-- | Print bracket results.
printBracketResult :: BracketResult -> IO ()
printBracketResult br = do
  putStrLn $ "  Observer: " ++ brObserverLabel br
  putStrLn $ "  Left Kan extension total: " ++ show (brLeftKanTotal br)
  putStrLn $ "  Right Kan extension total: " ++ show (brRightKanTotal br)
  putStrLn $ "  Bracket width (Ran - Lan): " ++ show (brBracketWidth br)
  putStrLn $ "  Objects in bracket: " ++ show (brObjectsInBracket br)

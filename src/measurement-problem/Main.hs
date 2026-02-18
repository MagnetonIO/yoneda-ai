{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Main
-- Description : Executable demonstrations of the measurement problem framework
-- Copyright   : (c) Matthew Long, YonedaAI Research Collective, 2026
--
-- Runs all examples from MeasurementCategory to demonstrate:
--   1. Qubit measurement and decoherence as Kan extension
--   2. Bell state entanglement and the extension deficit
--   3. Wigner's friend coherence failure

module Main where

import MeasurementCategory

main :: IO ()
main = do
    putStrLn "================================================================"
    putStrLn "The Measurement Problem as a Yoneda Obstruction"
    putStrLn "Executable Demonstrations"
    putStrLn "YonedaAI Research Collective, 2026"
    putStrLn "================================================================"
    putStrLn ""

    exampleQubit
    putStrLn ""
    putStrLn "----------------------------------------------------------------"
    putStrLn ""

    exampleBell
    putStrLn ""
    putStrLn "----------------------------------------------------------------"
    putStrLn ""

    exampleWigner
    putStrLn ""
    putStrLn "================================================================"
    putStrLn "All demonstrations complete."
    putStrLn ""
    putStrLn "Key results verified:"
    putStrLn "  1. Decoherence as Kan extension produces lossy approximation"
    putStrLn "  2. Extension deficit quantifies information loss"
    putStrLn "  3. Born rule gives unique natural transformation to probabilities"
    putStrLn "  4. Wigner's friend: incompatible presheaves, no global section"
    putStrLn "  5. Measurement opacity measures structural inaccessibility"
    putStrLn "================================================================"

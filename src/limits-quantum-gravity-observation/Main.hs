{-# LANGUAGE ScopedTypeVariables #-}

-- | Main module: demonstrates the Yoneda obstructions for quantum gravity
--
-- Evaluates multiple observational scenarios and computes extension deficits.

module Main where

import QGMeasurementCategory
import YonedaObstructions

-- ============================================================
-- Main: Run all scenarios
-- ============================================================

main :: IO ()
main = do
  putStrLn "=================================================================="
  putStrLn "Limits of Quantum Gravity Observation"
  putStrLn "Yoneda Constraint Analysis"
  putStrLn "=================================================================="
  putStrLn ""

  -- Physical constants
  putStrLn "--- Physical Constants ---"
  putStrLn $ "Planck length:  " ++ show planckLength ++ " m"
  putStrLn $ "Planck energy:  " ++ show planckEnergy ++ " J"
  putStrLn $ "Planck time:    " ++ show planckTime   ++ " s"
  putStrLn ""

  -- Planck resolution limit
  putStrLn "--- Planck Resolution Limit ---"
  putStrLn $ "Optimal probe energy: " ++ show optimalProbeEnergy ++ " J"
  putStrLn $ "Minimum resolvable scale: " ++ show (planckResolutionLimit optimalProbeEnergy) ++ " m"
  putStrLn $ "  (= " ++ show (planckResolutionLimit optimalProbeEnergy / planckLength) ++ " l_P)"
  putStrLn ""

  -- Evaluate scenarios
  putStrLn "=================================================================="
  putStrLn "OBSERVATIONAL SCENARIOS"
  putStrLn "=================================================================="

  mapM_ analyzeScenario
    [ ligoScenario
    , bmvScenario
    , planckScenario
    , blackHoleScenario
    ]

  -- Scale regime analysis
  putStrLn "=================================================================="
  putStrLn "SCALE REGIME ANALYSIS"
  putStrLn "=================================================================="
  putStrLn ""

  let testObjects = map scenarioObserver
        [ligoScenario, bmvScenario, planckScenario, blackHoleScenario]
  let names = ["LIGO", "BMV", "Planck Probe", "Black Hole"]
  mapM_ (\(name, obj) -> do
    let (regime, obsts, deficit) = regimeAnalysis obj
    putStrLn $ name ++ ":"
    putStrLn $ "  Regime: " ++ show regime
    putStrLn $ "  Total deficit: " ++ show (deficitTotal deficit) ++ " nats"
    putStrLn $ "  Trans-Planckian? " ++ show (transPlanckianCensorship obj)
    putStrLn ""
    ) (zip names testObjects)

  -- LQG area gap
  putStrLn "--- LQG Area Gap ---"
  -- NOTE: gamma = 0.2375 is the standard Barbero-Immirzi parameter
  -- derived from matching black hole entropy with Bekenstein-Hawking.
  -- This is a free parameter of LQG, not derived from first principles.
  let gamma = 0.2375  -- Barbero-Immirzi parameter (from BH entropy matching)
  putStrLn $ "Barbero-Immirzi parameter: " ++ show gamma
  putStrLn $ "Minimum area: " ++ show (lqgAreaGap gamma) ++ " m^2"
  putStrLn $ "  (= " ++ show (lqgAreaGap gamma / planckLength ^ (2 :: Int)) ++ " l_P^2)"
  putStrLn ""

  -- String scale
  putStrLn "--- String Theory Minimum Scale ---"
  let alphaPrime = (10 * planckLength) ^ (2 :: Int)  -- String length ~ 10 l_P
  putStrLn $ "alpha' = " ++ show alphaPrime ++ " m^2"
  putStrLn $ "Minimum scale: " ++ show (stringMinScale alphaPrime) ++ " m"
  putStrLn $ "  (= " ++ show (stringMinScale alphaPrime / planckLength) ++ " l_P)"
  putStrLn ""

  -- Ryu-Takayanagi example
  putStrLn "--- Ryu-Takayanagi Deficit (AdS_3/CFT_2) ---"
  let cCharge = 100    -- Central charge
  let intLen  = 1e-3   -- 1 mm boundary interval
  let uvCut   = planckLength
  let rtDef   = ryuTakayanagiDeficit cCharge intLen uvCut
  putStrLn $ "Central charge: " ++ show cCharge
  putStrLn $ "Interval length: " ++ show intLen ++ " m"
  putStrLn $ "RT deficit: " ++ show rtDef ++ " nats"
  putStrLn ""

  putStrLn "=================================================================="
  putStrLn "SUMMARY: Three Independent Obstructions"
  putStrLn "=================================================================="
  putStrLn "1. Gravitational Epistemic Horizon: BH formation at E >= c^4 L/(2G)"
  putStrLn "   -> Resolution limit: delta_x >= 2 l_P"
  putStrLn "2. Diffeomorphism Gauge Obstruction: Diff(M) quotient"
  putStrLn "   -> Observables reduced to gauge-invariant sector"
  putStrLn "3. Holographic Saturation Bound: S <= A/(4 G hbar)"
  putStrLn "   -> Extension deficit scales with boundary area"
  putStrLn ""
  putStrLn "These obstructions compose: all three apply simultaneously"
  putStrLn "in any quantum gravitational measurement scenario."

-- | Analyze a single scenario
analyzeScenario :: ObservationalScenario -> IO ()
analyzeScenario scenario = do
  putStrLn ""
  putStrLn $ "--- " ++ scenarioName scenario ++ " ---"
  putStrLn $ "Probe energy: " ++ show (scenarioProbeEnergy scenario) ++ " J"
  putStrLn $ "Probe scale:  " ++ show (scenarioProbeScale scenario) ++ " m"

  let obs  = scenarioObserver scenario
  let obsts = totalObstruction obs

  putStrLn ""
  putStrLn "Obstructions:"
  mapM_ (\o -> do
    putStrLn $ "  " ++ show (obstructionType o)
    putStrLn $ "    Magnitude: " ++ show (obstructionMagnitude o) ++ " nats"
    putStrLn $ "    Saturated: " ++ show (obstructionSaturated o)
    putStrLn $ "    " ++ obstructionDescription o
    ) obsts

  let deficit = evaluateScenario scenario
  putStrLn ""
  putStrLn "Extension Deficit:"
  putStrLn $ "  Quantum:       " ++ show (deficitQuantum deficit)
  putStrLn $ "  Gravitational: " ++ show (deficitGravitational deficit)
  putStrLn $ "  Gauge:         " ++ show (deficitGauge deficit)
  putStrLn $ "  Holographic:   " ++ show (deficitHolographic deficit)
  putStrLn $ "  TOTAL:         " ++ show (deficitTotal deficit) ++ " nats"
  putStrLn ""

-- Re-export for use in Main
optimalProbeEnergy :: Double
optimalProbeEnergy = planckEnergy / sqrt 2

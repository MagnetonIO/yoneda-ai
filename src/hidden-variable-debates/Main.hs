-- | Main executable for the hidden variable debates analysis.
-- Demonstrates the key results from the paper computationally:
-- 1. Kochen-Specker obstruction (Peres-Mermin magic square)
-- 2. Bell inequality analysis (CHSH violation)
-- 3. Kan extension deficit computation
-- 4. Hidden variable model classification
module Main where

import KochenSpecker (runKSVerification, KSResult(..))
import Bell (runBellAnalysis, BellResult(..))
import KanExtension (runKanAnalysis, KanResult(..))
import HiddenVariable (runHVAnalysis, HVAnalysis(..))
import MeasurementCategory (vonNeumannEntropy, singletReducedA,
                            pureState, maxMixed, isPure)
import qualified Data.Set as Set

main :: IO ()
main = do
  putStrLn "============================================================"
  putStrLn "  Hidden Variable Debates: Yoneda Constraint Analysis"
  putStrLn "  Computational Verification"
  putStrLn "============================================================"
  putStrLn ""

  -- 1. Kochen-Specker verification
  putStrLn "--- Section 1: Kochen-Specker Obstruction ---"
  putStrLn ""
  let ks = runKSVerification
  putStrLn $ "Algebraic contradiction in Peres-Mermin square: "
    ++ show (ksAlgebraicContradiction ks)
  putStrLn $ "No global section exists (KS obstruction): "
    ++ show (ksNoGlobalSection ks)
  putStrLn ""
  putStrLn $ ksInterpretation ks
  putStrLn ""

  -- 2. Bell inequality analysis
  putStrLn "--- Section 2: Bell Inequality (CHSH) Analysis ---"
  putStrLn ""
  let bell = runBellAnalysis
  putStrLn $ "Quantum CHSH value (optimal settings): "
    ++ show (brQuantumCHSH bell)
  putStrLn $ "Classical bound: "
    ++ show (brClassicalBound bell)
  putStrLn $ "Tsirelson bound: "
    ++ show (brTsirelsonBound bell)
  putStrLn $ "Bell violation detected: "
    ++ show (brViolation bell)
  putStrLn $ "Violation amount: "
    ++ show (brViolationAmount bell)
  putStrLn $ "Best local HV model CHSH: "
    ++ show (brLHVBestCHSH bell)
  putStrLn $ "Extension deficit (entropy of reduced state): "
    ++ show (brExtensionDeficit bell) ++ " bits"
  putStrLn ""
  putStrLn $ brInterpretation bell
  putStrLn ""

  -- 3. Kan extension deficit
  putStrLn "--- Section 3: Kan Extension Deficit ---"
  putStrLn ""
  let kan = runKanAnalysis
  putStrLn $ "Singlet state deficit: "
    ++ show (krSingletDeficit kan) ++ " bits"
  putStrLn $ "Product state deficit: "
    ++ show (krProductDeficit kan) ++ " bits"
  putStrLn ""
  putStrLn "Convergence (mixing parameter, deficit):"
  mapM_ (\(p, d) ->
    putStrLn $ "  p = " ++ show p ++ " -> deficit = " ++ show d
    ) (krConvergence kan)
  putStrLn ""
  putStrLn $ krInterpretation kan
  putStrLn ""

  -- 4. Hidden variable model classification
  putStrLn "--- Section 4: Hidden Variable Model Classification ---"
  putStrLn ""
  let hv = runHVAnalysis
  putStrLn $ "Viable models: " ++ show (hvaViableCount hv)
    ++ " out of " ++ show (length (hvaModels hv))
  putStrLn ""
  putStrLn $ hvaInterpretation hv
  putStrLn ""

  -- 5. Summary
  putStrLn "--- Summary ---"
  putStrLn ""
  putStrLn "The Yoneda Constraint on Observer Knowledge provides a"
  putStrLn "unified framework for understanding the hidden variable debates:"
  putStrLn ""
  putStrLn "  1. Hidden variables = features outside the representable presheaf"
  putStrLn "  2. No-go theorems = presheaf obstructions to functorial completions"
  putStrLn "  3. Extension deficit = quantitative measure of 'hiddenness'"
  putStrLn "  4. Viable HV theories must be contextual, non-local, psi-ontic"
  putStrLn "  5. The HV question is structurally undecidable from within QM"
  putStrLn ""
  putStrLn "============================================================"
  putStrLn "  Analysis complete."
  putStrLn "============================================================"

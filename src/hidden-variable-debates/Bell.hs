{-# LANGUAGE ScopedTypeVariables #-}

-- | CHSH inequality computation and Bell violation detection.
-- Implements the connection between presheaf non-factorizability
-- and Bell inequality violations within the Yoneda Constraint framework.
module Bell where

import Data.Complex
import MeasurementCategory

-- | Measurement settings for a Bell experiment.
-- Each setting is an angle (radians) for a spin measurement direction.
data BellSettings = BellSettings
  { aliceAngle1 :: Double
  , aliceAngle2 :: Double
  , bobAngle1   :: Double
  , bobAngle2   :: Double
  } deriving (Show)

-- | The optimal CHSH settings: angles separated by pi/8.
optimalCHSH :: BellSettings
optimalCHSH = BellSettings
  { aliceAngle1 = 0
  , aliceAngle2 = pi / 4
  , bobAngle1   = pi / 8
  , bobAngle2   = 3 * pi / 8
  }

-- | Compute the spin measurement operator along angle theta in the XZ plane.
-- sigma(theta) = cos(theta) * sigma_z + sin(theta) * sigma_x
spinOperator :: Double -> Matrix2
spinOperator theta =
  let c = cos theta :+ 0
      s = sin theta :+ 0
  in ((c, s), (s, negate c))

-- | Quantum correlation E(a, b) for the singlet state.
-- E(a, b) = -cos(a - b) for the singlet.
singletCorrelation :: Double -> Double -> Double
singletCorrelation a b = -cos (a - b)

-- | Compute the CHSH value for given settings using quantum correlations.
chshValueQuantum :: BellSettings -> Double
chshValueQuantum settings =
  let e11 = singletCorrelation (aliceAngle1 settings) (bobAngle1 settings)
      e12 = singletCorrelation (aliceAngle1 settings) (bobAngle2 settings)
      e21 = singletCorrelation (aliceAngle2 settings) (bobAngle1 settings)
      e22 = singletCorrelation (aliceAngle2 settings) (bobAngle2 settings)
  in abs (e11 - e12) + abs (e21 + e22)

-- | Classical (local hidden variable) CHSH bound.
classicalBound :: Double
classicalBound = 2.0

-- | Tsirelson's bound (quantum maximum).
tsirelsonBound :: Double
tsirelsonBound = 2 * sqrt 2

-- | Check if a CHSH value violates the classical bound.
violatesBell :: Double -> Bool
violatesBell value = value > classicalBound + 1e-10

-- | A local hidden variable model for a Bell experiment.
-- Lambda is the hidden variable, A and B are deterministic response functions.
data LocalHVModel = LocalHVModel
  { hvLambdas  :: [Double]        -- ^ Hidden variable values
  , hvWeights  :: [Double]        -- ^ Probability weights
  , hvResponseA :: Double -> Double -> Double  -- ^ A(angle, lambda) -> {-1, +1}
  , hvResponseB :: Double -> Double -> Double  -- ^ B(angle, lambda) -> {-1, +1}
  }

-- | Compute CHSH value for a local hidden variable model.
chshValueLHV :: LocalHVModel -> BellSettings -> Double
chshValueLHV model settings =
  let lambdas = hvLambdas model
      weights = hvWeights model
      respA = hvResponseA model
      respB = hvResponseB model
      correlation a b = sum $ zipWith (\l w ->
        w * respA a l * respB b l
        ) lambdas weights
      e11 = correlation (aliceAngle1 settings) (bobAngle1 settings)
      e12 = correlation (aliceAngle1 settings) (bobAngle2 settings)
      e21 = correlation (aliceAngle2 settings) (bobAngle1 settings)
      e22 = correlation (aliceAngle2 settings) (bobAngle2 settings)
  in abs (e11 - e12) + abs (e21 + e22)

-- | A simple deterministic local HV model.
-- Lambda is a unit vector direction; response is sign(a . lambda).
simpleLHVModel :: Int -> LocalHVModel
simpleLHVModel n =
  let lambdas = [2 * pi * fromIntegral i / fromIntegral n | i <- [0..n-1]]
      weights = replicate n (1.0 / fromIntegral n)
      respA angle lambda = if cos (angle - lambda) >= 0 then 1.0 else (-1.0)
      respB angle lambda = if cos (angle - lambda) >= 0 then (-1.0) else 1.0
  in LocalHVModel lambdas weights respA respB

-- | Full Bell test result.
data BellResult = BellResult
  { brQuantumCHSH      :: Double
  , brClassicalBound    :: Double
  , brTsirelsonBound    :: Double
  , brViolation         :: Bool
  , brViolationAmount   :: Double
  , brLHVBestCHSH       :: Double
  , brExtensionDeficit  :: Double  -- ^ Von Neumann entropy of reduced state
  , brInterpretation    :: String
  } deriving (Show)

-- | Run the Bell inequality analysis.
runBellAnalysis :: BellResult
runBellAnalysis =
  let settings = optimalCHSH
      qValue = chshValueQuantum settings
      -- Test local HV models with increasing resolution
      lhvValues = [chshValueLHV (simpleLHVModel n) settings | n <- [10, 100, 1000]]
      bestLHV = maximum lhvValues
      -- Extension deficit = entropy of reduced state
      deficit = vonNeumannEntropy singletReducedA
  in BellResult
    { brQuantumCHSH = qValue
    , brClassicalBound = classicalBound
    , brTsirelsonBound = tsirelsonBound
    , brViolation = violatesBell qValue
    , brViolationAmount = qValue - classicalBound
    , brLHVBestCHSH = bestLHV
    , brExtensionDeficit = deficit
    , brInterpretation = unlines
        [ "Bell inequality analysis (CHSH):"
        , "  Quantum CHSH value: " ++ show qValue
        , "  Classical bound: " ++ show classicalBound
        , "  Tsirelson bound: " ++ show tsirelsonBound
        , "  Violation: " ++ show (violatesBell qValue)
        , "  Violation amount: " ++ show (qValue - classicalBound)
        , ""
        , "  Best LHV model CHSH: " ++ show bestLHV
        , "  Extension deficit (entropy): " ++ show deficit
        , ""
        , "Yoneda interpretation:"
        , "  The singlet state's representable presheaf does not factorize"
        , "  (entanglement = non-factorizability, Prop. 5.7 of YC-v2)."
        , "  Any local HV model produces a factorizable presheaf,"
        , "  hence CHSH <= 2. The gap (2*sqrt(2) - 2) = " ++
          show (tsirelsonBound - classicalBound)
        , "  measures the maximum non-factorizability in Meas_Q."
        , "  The extension deficit Delta(S_A) = " ++ show deficit ++
          " bits"
        , "  quantifies the information Alice cannot access locally."
        ]
    }

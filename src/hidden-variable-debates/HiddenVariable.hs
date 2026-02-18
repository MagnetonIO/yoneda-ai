{-# LANGUAGE ScopedTypeVariables #-}

-- | Implementation of hidden variable models and functorial completions.
-- Demonstrates how different HV theories correspond to different
-- extensions of the observer's representable presheaf.
module HiddenVariable where

import Data.Complex
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import MeasurementCategory

-- | A hidden variable model: specifies additional variables lambda
-- and response functions for measurements.
data HVModel = HVModel
  { hvName      :: String
  , hvLambdaSpace :: [Double]          -- ^ Discrete lambda values
  , hvDistribution :: [Double]         -- ^ p(lambda)
  , hvResponse  :: String              -- ^ Type: "deterministic" or "stochastic"
  , hvLocal     :: Bool                -- ^ Is the model local?
  , hvContextual :: Bool               -- ^ Is the model contextual?
  , hvPsiOntic  :: Bool                -- ^ Is the model psi-ontic?
  } deriving (Show)

-- | A Bohmian-style hidden variable model for a single qubit.
-- Lambda = position on [0, 1), distributed as |psi(x)|^2.
bohmianModel :: HVModel
bohmianModel = HVModel
  { hvName = "Bohmian (pilot-wave)"
  , hvLambdaSpace = [fromIntegral i / 100 | i <- [0..99]]
  , hvDistribution = replicate 100 0.01  -- Uniform for maximally mixed
  , hvResponse = "deterministic"
  , hvLocal = False
  , hvContextual = True
  , hvPsiOntic = True
  }

-- | A Bell-type local hidden variable model.
bellLocalModel :: HVModel
bellLocalModel = HVModel
  { hvName = "Bell-type local HV"
  , hvLambdaSpace = [fromIntegral i * 2 * pi / 100 | i <- [0..99]]
  , hvDistribution = replicate 100 0.01
  , hvResponse = "deterministic"
  , hvLocal = True
  , hvContextual = False
  , hvPsiOntic = True
  }

-- | Check which no-go theorems a model violates.
data NoGoStatus = NoGoStatus
  { ngsVonNeumann    :: Bool  -- ^ Violates von Neumann? (always True for deterministic)
  , ngsKochenSpecker :: Bool  -- ^ Violates KS? (True if non-contextual)
  , ngsBell          :: Bool  -- ^ Violates Bell? (True if local + deterministic)
  , ngsPBR           :: Bool  -- ^ Violates PBR? (True if psi-epistemic)
  , ngsFreeWill      :: Bool  -- ^ Violates FW? (True if deterministic, absent superdeterminism)
  } deriving (Show)

-- | Evaluate which no-go theorems constrain a given model.
checkNoGo :: HVModel -> NoGoStatus
checkNoGo model = NoGoStatus
  { ngsVonNeumann = False  -- Von Neumann's linearity assumption is too strong
  , ngsKochenSpecker = not (hvContextual model)
  , ngsBell = hvLocal model && hvResponse model == "deterministic"
  , ngsPBR = not (hvPsiOntic model)
  , ngsFreeWill = hvResponse model == "deterministic"
    -- (without superdeterminism; FW theorem applies if MIN holds)
  }

-- | Check if a model is viable (satisfies all no-go constraints).
isViable :: HVModel -> Bool
isViable model =
  let ngs = checkNoGo model
  in not (ngsKochenSpecker ngs)
     && not (ngsBell ngs)
     && not (ngsPBR ngs)
     -- Free Will theorem doesn't strictly rule out the model
     -- if one accepts that particles' responses are "free" too

-- | Classify a model by its functorial properties.
data FunctorialClassification = FunctorialClassification
  { fcType           :: String  -- ^ "deterministic", "stochastic", etc.
  , fcFactorizable   :: Bool    -- ^ Does the extended presheaf factorize?
  , fcGlobalSections :: Bool    -- ^ Does the completion have global sections?
  , fcKanCompatible  :: Bool    -- ^ Is it compatible with the Kan extension?
  } deriving (Show)

classifyModel :: HVModel -> FunctorialClassification
classifyModel model = FunctorialClassification
  { fcType = hvResponse model
  , fcFactorizable = hvLocal model
  , fcGlobalSections = not (hvContextual model)
  , fcKanCompatible = isViable model
  }

-- | Simulate a Bohmian trajectory for a 1D particle.
-- Uses the guidance equation: v = (hbar/m) * Im(psi'/psi)
-- For a superposition psi = c1*phi1 + c2*phi2.
data BohmTrajectory = BohmTrajectory
  { btPositions :: [Double]
  , btTimes     :: [Double]
  , btVelocities :: [Double]
  } deriving (Show)

-- | Simple Bohmian trajectory for a Gaussian wave packet.
-- psi(x, t) = (1/(2*pi*sigma^2))^(1/4) * exp(-(x-x0)^2/(4*sigma^2) + i*k*x)
-- Guidance velocity: v = hbar*k/m (constant for plane wave component)
simulateBohmTrajectory :: Double  -- ^ Initial position
                       -> Double  -- ^ Wave number k
                       -> Double  -- ^ Mass (in natural units)
                       -> Double  -- ^ Time step
                       -> Int     -- ^ Number of steps
                       -> BohmTrajectory
simulateBohmTrajectory x0 k m dt nSteps =
  let v = k / m  -- hbar = 1 in natural units
      times = [fromIntegral i * dt | i <- [0..nSteps]]
      positions = [x0 + v * t | t <- times]
      velocities = replicate (nSteps + 1) v
  in BohmTrajectory positions times velocities

-- | Analysis result for hidden variable theories.
data HVAnalysis = HVAnalysis
  { hvaModels        :: [(HVModel, NoGoStatus, FunctorialClassification)]
  , hvaViableCount   :: Int
  , hvaInterpretation :: String
  } deriving (Show)

-- | Run the hidden variable analysis.
runHVAnalysis :: HVAnalysis
runHVAnalysis =
  let models = [bohmianModel, bellLocalModel]
      analyzed = [(m, checkNoGo m, classifyModel m) | m <- models]
      viable = length [() | (m, _, _) <- analyzed, isViable m]
  in HVAnalysis
    { hvaModels = analyzed
    , hvaViableCount = viable
    , hvaInterpretation = unlines
        [ "Hidden Variable Theory Analysis:"
        , ""
        , "Models analyzed:"
        , "  1. Bohmian (pilot-wave): deterministic, non-local, contextual, psi-ontic"
        , "     -> VIABLE (satisfies all no-go constraints)"
        , "     -> Non-locality is necessary to bridge the extension deficit"
        , ""
        , "  2. Bell-type local HV: deterministic, local, non-contextual, psi-ontic"
        , "     -> NOT VIABLE (violates Bell + Kochen-Specker)"
        , "     -> Factorizable presheaf cannot reproduce entangled correlations"
        , ""
        , "Yoneda perspective:"
        , "  Hidden variables correspond to functorial completions of"
        , "  the observer's representable presheaf y^(S, sigma_S)."
        , "  The no-go theorems constrain which completions are possible:"
        , "  - Von Neumann: no globally linear completion (too strong)"
        , "  - KS: no non-contextual completion (no global sections)"
        , "  - Bell: no local factorizable completion"
        , "  - PBR: completion must be psi-ontic (disjoint fibers)"
        , "  What remains: contextual, non-local, psi-ontic completions"
        , "  = exactly what Bohmian mechanics provides."
        ]
    }

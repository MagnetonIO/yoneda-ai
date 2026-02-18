{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : MeasurementCategory
-- Description : Category-theoretic models of the quantum measurement problem
-- Copyright   : (c) Matthew Long, YonedaAI Research Collective, 2026
-- License     : BSD-3-Clause
--
-- This module implements the core categorical structures from
-- "The Measurement Problem as a Yoneda Obstruction" including:
--
--   * The quantum measurement category (MeasQ)
--   * The classical outcome category (MeasC)
--   * The decoherence functor as a Kan extension
--   * The Born rule as a natural transformation
--   * The measurement obstruction computation
--   * Wigner's friend coherence analysis

module MeasurementCategory
  ( -- * Quantum States
    QuantumState(..)
  , DensityMatrix(..)
  , mkPure
  , mkMixed
  , partialTrace
  , vonNeumannEntropy

    -- * Measurement Categories
  , MeasQ(..)
  , MeasC(..)
  , POVM(..)
  , MeasurementOutcome(..)

    -- * Functors
  , decoherenceFunctor
  , inclusionFunctor
  , transitionFunctor

    -- * Born Rule
  , bornRule
  , bornProbabilities

    -- * Yoneda Presheaf
  , Presheaf(..)
  , representablePresheaf
  , yonedaEmbedding

    -- * Measurement Obstruction
  , extensionDeficit
  , deficitEntropy
  , measurementOpacity

    -- * Wigner's Friend
  , WignerScenario(..)
  , friendPresheaf
  , wignerPresheaf
  , coherenceFailure

    -- * Examples
  , exampleQubit
  , exampleBell
  , exampleWigner
  ) where

import Data.Complex
import Data.List (foldl', transpose)

-- ============================================================
-- Quantum States
-- ============================================================

-- | A quantum state represented as a density matrix.
-- For an n-dimensional Hilbert space, this is an n x n matrix
-- of complex numbers.
data DensityMatrix = DensityMatrix
  { dmDim    :: Int
  , dmMatrix :: [[Complex Double]]
  } deriving (Show)

-- | A quantum state can be pure or mixed.
data QuantumState
  = PureState [Complex Double]       -- ^ State vector |psi>
  | MixedState DensityMatrix         -- ^ Density matrix rho
  deriving (Show)

-- | Construct a pure state density matrix |psi><psi|
mkPure :: [Complex Double] -> DensityMatrix
mkPure psi = DensityMatrix n matrix
  where
    n = length psi
    matrix = [[psi !! i * conjugate (psi !! j) | j <- [0..n-1]] | i <- [0..n-1]]

-- | Construct a mixed state from probabilities and pure states
mkMixed :: [(Double, [Complex Double])] -> DensityMatrix
mkMixed components = DensityMatrix n sumMatrix
  where
    n = dmDim (mkPure (snd (head components)))
    matrices = [scaleMatrix p (mkPure v) | (p, v) <- components]
    sumMatrix = foldl' addMatrices (zeroMatrix n) (map dmMatrix matrices)

-- | Partial trace over the environment (second subsystem).
-- Given rho on H_S ⊗ H_E with dimensions d_S and d_E,
-- returns the reduced density matrix on H_S.
partialTrace :: Int -> Int -> DensityMatrix -> DensityMatrix
partialTrace dS dE (DensityMatrix _ rho) = DensityMatrix dS reduced
  where
    reduced = [[traceBlock i j | j <- [0..dS-1]] | i <- [0..dS-1]]
    traceBlock i j = sum [rho !! (i * dE + k) !! (j * dE + k) | k <- [0..dE-1]]

-- | Von Neumann entropy S(rho) = -Tr(rho log rho)
vonNeumannEntropy :: DensityMatrix -> Double
vonNeumannEntropy dm = negate $ sum [entropyTerm ev | ev <- eigenvalues dm]
  where
    entropyTerm x
      | x <= 1e-15 = 0
      | otherwise   = x * log x

-- | Compute eigenvalues of a 2x2 Hermitian matrix (simplified).
-- For general matrices, a full eigensolver would be needed.
eigenvalues :: DensityMatrix -> [Double]
eigenvalues (DensityMatrix 2 [[a, b], [c, d]]) =
    let tr = realPart a + realPart d
        det = realPart a * realPart d - realPart (b * c)
        disc = sqrt (max 0 (tr * tr - 4 * det))
    in [(tr + disc) / 2, (tr - disc) / 2]
eigenvalues (DensityMatrix 1 [[a]]) = [realPart a]
eigenvalues (DensityMatrix n m)
    | isApproxDiagonal n m = [realPart (m !! i !! i) | i <- [0..n-1]]
    | otherwise = error $
        "eigenvalues: exact diagonalization not implemented for dim " ++ show n
        ++ ". Use dim <= 2 or ensure the matrix is approximately diagonal."

-- | Check whether a matrix is approximately diagonal (off-diagonal < epsilon).
isApproxDiagonal :: Int -> [[Complex Double]] -> Bool
isApproxDiagonal n m = all (< 1e-10)
    [ magnitude (m !! i !! j) | i <- [0..n-1], j <- [0..n-1], i /= j ]

-- ============================================================
-- Measurement Categories
-- ============================================================

-- | An object in the quantum measurement category MeasQ.
-- Consists of a subsystem label, Hilbert space dimension, and state.
data MeasQ = MeasQ
  { mqLabel :: String
  , mqDim   :: Int
  , mqState :: DensityMatrix
  } deriving (Show)

-- | An object in the classical measurement category MeasC.
-- Consists of a label and a classical probability distribution.
data MeasC = MeasC
  { mcLabel :: String
  , mcProbs :: [Double]
  } deriving (Show)

-- | A POVM (Positive Operator-Valued Measure) element.
data POVM = POVM
  { povmLabel    :: String
  , povmElements :: [DensityMatrix]  -- ^ POVM elements E_i with sum E_i = I
  } deriving (Show)

-- | A measurement outcome with associated probability.
data MeasurementOutcome = MeasurementOutcome
  { moIndex :: Int
  , moProb  :: Double
  , moLabel :: String
  } deriving (Show)

-- ============================================================
-- The Born Rule (Natural Transformation)
-- ============================================================

-- | The Born rule: p_i = Tr(rho * E_i)
-- This is the unique natural transformation from the representable
-- presheaf to the probability functor (Theorem 6.2 in the paper).
bornRule :: DensityMatrix -> DensityMatrix -> Double
bornRule rho ei = realPart $ trace (matMul (dmMatrix rho) (dmMatrix ei))

-- | Compute Born probabilities for all POVM elements.
bornProbabilities :: DensityMatrix -> POVM -> [MeasurementOutcome]
bornProbabilities rho povm =
    [ MeasurementOutcome i (bornRule rho ei) (povmLabel povm ++ "_" ++ show i)
    | (i, ei) <- zip [0..] (povmElements povm)
    ]

-- ============================================================
-- Decoherence Functor (Kan Extension)
-- ============================================================

-- | The decoherence functor D: MeasQ -> MeasQ
-- Implements dephasing in the pointer basis (Definition 5.1).
-- This is the left Kan extension of id_{MeasC} along the inclusion
-- iota: MeasC -> MeasQ (Theorem 5.2).
decoherenceFunctor :: MeasQ -> MeasQ
decoherenceFunctor (MeasQ label dim (DensityMatrix n m)) =
    MeasQ label dim (DensityMatrix n decohered)
  where
    decohered = [[if i == j then m !! i !! j else 0 | j <- [0..n-1]] | i <- [0..n-1]]

-- | The inclusion functor iota: MeasC -> MeasQ
-- Embeds classical probability distributions as diagonal density matrices.
inclusionFunctor :: MeasC -> MeasQ
inclusionFunctor (MeasC label probs) =
    MeasQ label n (DensityMatrix n diag)
  where
    n = length probs
    diag = [[if i == j then (probs !! i) :+ 0 else 0 | j <- [0..n-1]] | i <- [0..n-1]]

-- | The quantum-to-classical transition functor T = pi_C . D
-- Composition of decoherence and projection onto classical subcategory.
transitionFunctor :: MeasQ -> MeasC
transitionFunctor (MeasQ label _ dm) =
    MeasC label [realPart (dmMatrix dm' !! i !! i) | i <- [0..dmDim dm' - 1]]
  where
    dm' = mqState (decoherenceFunctor (MeasQ label (dmDim dm) dm))

-- ============================================================
-- Yoneda Presheaf (Representable Functor)
-- ============================================================

-- | A presheaf on the measurement category.
-- In Haskell, we model this as a function from objects to sets,
-- together with restriction maps.
data Presheaf = Presheaf
  { pshLabel :: String
  , pshApply :: MeasQ -> [MeasurementOutcome]
    -- ^ For each object, the set of measurement outcomes
  }

-- | The representable presheaf Hom(A, -)
-- For a quantum observer A, this assigns to each object B
-- the set of CPTP maps from A to B, realized here as
-- the set of measurement outcomes obtainable from A.
representablePresheaf :: MeasQ -> Presheaf
representablePresheaf observer = Presheaf
  { pshLabel = "y_{" ++ mqLabel observer ++ "}"
  , pshApply = \target ->
      let povm = standardPOVM (mqDim (mqState target))
      in bornProbabilities (mqState observer) povm
  }

-- | The Yoneda embedding y: MeasQ -> PSh(MeasQ)
-- Maps each object to its representable presheaf.
yonedaEmbedding :: MeasQ -> Presheaf
yonedaEmbedding = representablePresheaf

-- ============================================================
-- Measurement Obstruction
-- ============================================================

-- | Compute the extension deficit (Definition 5.3).
-- Delta = S(D(rho)) - S(rho) >= 0
-- Measures the information lost in the decoherence Kan extension.
extensionDeficit :: MeasQ -> Double
extensionDeficit mq =
    vonNeumannEntropy decoheredState - vonNeumannEntropy (mqState mq)
  where
    decoheredState = mqState (decoherenceFunctor mq)

-- | Synonym for extensionDeficit.
deficitEntropy :: MeasQ -> Double
deficitEntropy = extensionDeficit

-- | Measurement opacity index (Definition 8.1 from the MBP paper).
-- kappa = 1 - dim(emergent) / dim(pre-geometric)
-- Here approximated as the ratio of classical to quantum information.
measurementOpacity :: MeasQ -> Double
measurementOpacity mq =
    let sQuantum = vonNeumannEntropy (mqState mq)
        sClassical = vonNeumannEntropy (mqState (decoherenceFunctor mq))
        logDim = log (fromIntegral (mqDim (mqState mq)))
    in if logDim > 0
       then (sClassical - sQuantum) / logDim
       else 0

-- ============================================================
-- Wigner's Friend (2-Categorical Coherence)
-- ============================================================

-- | A Wigner's friend scenario.
data WignerScenario = WignerScenario
  { wsQubit     :: MeasQ    -- ^ The qubit being measured
  , wsFriend    :: MeasQ    -- ^ Friend's state after measurement
  , wsWigner    :: MeasQ    -- ^ Wigner's state (sees entangled system)
  , wsFriendSaw :: Maybe Int  -- ^ Which outcome the friend saw (if any)
  } deriving (Show)

-- | The Friend's representable presheaf (post-measurement, definite outcome).
friendPresheaf :: WignerScenario -> Presheaf
friendPresheaf ws = representablePresheaf (wsFriend ws)

-- | Wigner's representable presheaf (pre-measurement, superposition).
wignerPresheaf :: WignerScenario -> Presheaf
wignerPresheaf ws = representablePresheaf (wsWigner ws)

-- | Compute the coherence failure between Friend and Wigner presheaves.
-- Returns a measure of incompatibility (Theorem 7.3).
-- A non-zero value indicates coherence failure in the 2-category.
coherenceFailure :: WignerScenario -> Double
coherenceFailure ws =
    let friendDeficit = extensionDeficit (wsFriend ws)
        wignerDeficit = extensionDeficit (wsWigner ws)
        -- The coherence failure is measured by the difference in
        -- extension deficits: Friend sees a pure (definite) state
        -- while Wigner sees a superposition.
    in abs (friendDeficit - wignerDeficit)

-- ============================================================
-- Examples
-- ============================================================

-- | Example: a qubit in state |+> = (|0> + |1>)/sqrt(2)
exampleQubit :: IO ()
exampleQubit = do
    let psi = [1/sqrt 2 :+ 0, 1/sqrt 2 :+ 0]
        rho = mkPure psi
        mq = MeasQ "qubit" 2 rho

    putStrLn "=== Qubit Example ==="
    putStrLn $ "State: |+> = (|0> + |1>)/sqrt(2)"
    putStrLn $ "Density matrix dimension: " ++ show (dmDim rho)
    putStrLn $ "Von Neumann entropy: " ++ show (vonNeumannEntropy rho)
    putStrLn ""

    -- Decoherence
    let mqD = decoherenceFunctor mq
    putStrLn "After decoherence:"
    putStrLn $ "  Diagonal state entropy: " ++ show (vonNeumannEntropy (mqState mqD))
    putStrLn $ "  Extension deficit: " ++ show (extensionDeficit mq)
    putStrLn $ "  Measurement opacity: " ++ show (measurementOpacity mq)
    putStrLn ""

    -- Born rule
    let zPovm = standardPOVM 2
        outcomes = bornProbabilities rho zPovm
    putStrLn "Born rule probabilities (Z-basis):"
    mapM_ (\o -> putStrLn $ "  " ++ moLabel o ++ ": " ++ show (moProb o)) outcomes
    putStrLn ""

    -- Classical transition
    let mc = transitionFunctor mq
    putStrLn $ "Classical outcome: " ++ show (mcProbs mc)

-- | Example: a Bell state |Phi+> = (|00> + |11>)/sqrt(2)
exampleBell :: IO ()
exampleBell = do
    let psi = [1/sqrt 2 :+ 0, 0, 0, 1/sqrt 2 :+ 0]
        rho = mkPure psi
        -- Partial trace to get the reduced state of qubit A
        rhoA = partialTrace 2 2 rho
        mqA = MeasQ "qubit_A" 2 rhoA

    putStrLn "=== Bell State Example ==="
    putStrLn $ "State: |Phi+> = (|00> + |11>)/sqrt(2)"
    putStrLn $ "Reduced state of A (after partial trace):"
    putStrLn $ "  Von Neumann entropy: " ++ show (vonNeumannEntropy rhoA)
    putStrLn $ "  (Should be ln(2) ~ 0.693 for maximally entangled state)"
    putStrLn ""

    -- Extension deficit
    putStrLn "Measurement obstruction for observer A:"
    putStrLn $ "  Extension deficit: " ++ show (extensionDeficit mqA)
    putStrLn $ "  Measurement opacity: " ++ show (measurementOpacity mqA)
    putStrLn ""

    -- The observer cannot determine the global state
    putStrLn "Observer A's presheaf determines rho_A but NOT |Phi+>"
    putStrLn $ "  Hidden parameters: " ++ show (2*2 - 1) ++ " (dim H_E^2 - 1)"

-- | Example: Wigner's friend scenario
exampleWigner :: IO ()
exampleWigner = do
    putStrLn "=== Wigner's Friend Example ==="

    -- The qubit starts in |+>
    let psiQ = [1/sqrt 2 :+ 0, 1/sqrt 2 :+ 0]
        qubit = MeasQ "qubit" 2 (mkPure psiQ)

    -- After Friend measures: collapsed to |0> (say)
    let friendState = MeasQ "friend" 2 (mkPure [1 :+ 0, 0])

    -- Wigner sees entangled state: (|0>|F_0> + |1>|F_1>)/sqrt(2)
    -- Reduced state for Wigner (tracing over internal friend detail)
    -- is the maximally mixed state
    let wignerState = MeasQ "wigner" 2 (mkMixed [(0.5, [1 :+ 0, 0]), (0.5, [0, 1 :+ 0])])

    let scenario = WignerScenario qubit friendState wignerState (Just 0)

    putStrLn "Friend's perspective:"
    putStrLn $ "  State: |0> (definite outcome)"
    putStrLn $ "  Extension deficit: " ++ show (extensionDeficit friendState)
    putStrLn ""

    putStrLn "Wigner's perspective:"
    putStrLn $ "  State: maximally mixed (entangled with friend)"
    putStrLn $ "  Extension deficit: " ++ show (extensionDeficit wignerState)
    putStrLn ""

    putStrLn "Coherence failure (2-categorical):"
    putStrLn $ "  |Delta_Friend - Delta_Wigner| = " ++ show (coherenceFailure scenario)
    putStrLn "  (Non-zero => incompatible presheaves, no global section)"
    putStrLn ""
    putStrLn "Resolution: Friend and Wigner have DIFFERENT representable presheaves."
    putStrLn "No requirement for mutual consistency (Corollary 7.4)."

-- ============================================================
-- Internal Utilities
-- ============================================================

-- | Standard POVM in computational basis (projective measurement).
standardPOVM :: Int -> POVM
standardPOVM n = POVM "Z-basis" elements
  where
    elements = [DensityMatrix n (projector i n) | i <- [0..n-1]]
    projector i dim = [[if r == i && c == i then 1 else 0 | c <- [0..dim-1]] | r <- [0..dim-1]]

-- | Matrix multiplication for complex matrices.
matMul :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
matMul a b = [[sum [a !! i !! k * bT !! j !! k | k <- [0..cols-1]] | j <- [0..length bT - 1]] | i <- [0..rows-1]]
  where
    rows = length a
    cols = length (head a)
    bT = transpose b

-- | Trace of a complex matrix.
trace :: [[Complex Double]] -> Complex Double
trace m = sum [m !! i !! i | i <- [0..length m - 1]]

-- | Scale a density matrix by a real factor.
scaleMatrix :: Double -> DensityMatrix -> DensityMatrix
scaleMatrix p (DensityMatrix n m) =
    DensityMatrix n [[((p :+ 0) * m !! i !! j) | j <- [0..n-1]] | i <- [0..n-1]]

-- | Add two complex matrices.
addMatrices :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
addMatrices a b = [[a !! i !! j + b !! i !! j | j <- [0..cols-1]] | i <- [0..rows-1]]
  where
    rows = length a
    cols = length (head a)

-- | Zero matrix.
zeroMatrix :: Int -> [[Complex Double]]
zeroMatrix n = [[0 | _ <- [0..n-1]] | _ <- [0..n-1]]

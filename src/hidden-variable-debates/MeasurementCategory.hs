{-# LANGUAGE ScopedTypeVariables #-}

-- | Implementation of the quantum measurement category Meas_Q.
-- Objects are (System, DensityOperator) pairs.
-- Morphisms are CPTP maps (represented as Kraus operators).
module MeasurementCategory where

import Data.Complex
import Data.List (transpose)

-- | A 2x2 complex matrix (sufficient for qubit systems).
type Matrix2 = ((Complex Double, Complex Double),
                (Complex Double, Complex Double))

-- | A density operator for a qubit system.
data DensityOp = DensityOp
  { rho00 :: Complex Double
  , rho01 :: Complex Double
  , rho10 :: Complex Double
  , rho11 :: Complex Double
  } deriving (Show)

-- | Construct a pure state density operator |psi><psi|.
pureState :: Complex Double -> Complex Double -> DensityOp
pureState alpha beta = DensityOp
  { rho00 = alpha * conjugate alpha
  , rho01 = alpha * conjugate beta
  , rho10 = beta * conjugate alpha
  , rho11 = beta * conjugate beta
  }

-- | The spin-up state |0>.
spinUp :: DensityOp
spinUp = pureState 1 0

-- | The spin-down state |1>.
spinDown :: DensityOp
spinDown = pureState 0 1

-- | The plus state |+> = (|0> + |1>) / sqrt(2).
spinPlus :: DensityOp
spinPlus = pureState (1/sqrt 2 :+ 0) (1/sqrt 2 :+ 0)

-- | The maximally mixed state I/2.
maxMixed :: DensityOp
maxMixed = DensityOp (0.5 :+ 0) 0 0 (0.5 :+ 0)

-- | Trace of a density operator.
traceOp :: DensityOp -> Complex Double
traceOp rho = rho00 rho + rho11 rho

-- | Von Neumann entropy S(rho) = -Tr(rho log rho).
-- Computed via eigenvalues for 2x2 density matrices.
vonNeumannEntropy :: DensityOp -> Double
vonNeumannEntropy rho =
  let (l1, l2) = eigenvalues2x2 rho
      s x = if x > 1e-15 then -x * log x / log 2 else 0
  in s l1 + s l2

-- | Eigenvalues of a 2x2 density matrix.
eigenvalues2x2 :: DensityOp -> (Double, Double)
eigenvalues2x2 rho =
  let a = magnitude (rho00 rho)
      d = magnitude (rho11 rho)
      bc = magnitude (rho01 rho)
      tr = a + d
      det = a * d - bc * bc
      disc = sqrt (max 0 (tr * tr - 4 * det))
  in ((tr + disc) / 2, (tr - disc) / 2)

-- | A measurement object in Meas_Q.
data MeasObj = MeasObj
  { measSystem :: String        -- ^ System label
  , measState  :: DensityOp     -- ^ Density operator
  } deriving (Show)

-- | Pauli matrices.
sigmaX, sigmaY, sigmaZ :: Matrix2
sigmaX = ((0, 1), (1, 0))
sigmaY = ((0, 0 :+ (-1)), (0 :+ 1, 0))
sigmaZ = ((1, 0), (0, -1))

-- | Identity matrix.
identity2 :: Matrix2
identity2 = ((1, 0), (0, 1))

-- | Matrix-vector multiplication for 2x2.
matVecMul :: Matrix2 -> (Complex Double, Complex Double) -> (Complex Double, Complex Double)
matVecMul ((a,b),(c,d)) (x,y) = (a*x + b*y, c*x + d*y)

-- | Matrix multiplication for 2x2.
matMul :: Matrix2 -> Matrix2 -> Matrix2
matMul ((a,b),(c,d)) ((e,f),(g,h)) =
  ((a*e+b*g, a*f+b*h), (c*e+d*g, c*f+d*h))

-- | Expectation value Tr(rho * A) for a 2x2 observable.
expectationValue :: DensityOp -> Matrix2 -> Complex Double
expectationValue rho ((a,b),(c,d)) =
  rho00 rho * a + rho01 rho * c + rho10 rho * b + rho11 rho * d

-- | Born rule probability for outcome +1 of a binary observable.
bornProbPlus :: DensityOp -> Matrix2 -> Double
bornProbPlus rho obs =
  let ev = expectationValue rho obs
  in (1 + realPart ev) / 2

-- | Born rule probability for outcome -1 of a binary observable.
bornProbMinus :: DensityOp -> Matrix2 -> Double
bornProbMinus rho obs = 1 - bornProbPlus rho obs

-- | Check if a state is pure (Tr(rho^2) = 1).
isPure :: DensityOp -> Bool
isPure rho =
  let purity = magnitude (rho00 rho * rho00 rho + rho01 rho * rho10 rho
                         + rho10 rho * rho01 rho + rho11 rho * rho11 rho)
  in abs (purity - 1.0) < 1e-10

-- | Partial trace over the second subsystem of a 4x4 density matrix.
-- Input: 4x4 matrix as a list of lists.
-- Output: 2x2 reduced density matrix.
partialTraceB :: [[Complex Double]] -> DensityOp
partialTraceB mat =
  DensityOp
    { rho00 = (mat !! 0 !! 0) + (mat !! 1 !! 1)
    , rho01 = (mat !! 0 !! 2) + (mat !! 1 !! 3)
    , rho10 = (mat !! 2 !! 0) + (mat !! 3 !! 1)
    , rho11 = (mat !! 2 !! 2) + (mat !! 3 !! 3)
    }

-- | The singlet state |Psi-> = (|01> - |10>) / sqrt(2) as a 4x4 density matrix.
singletState :: [[Complex Double]]
singletState =
  let s = 1 / sqrt 2 :+ 0
      -- |Psi-> = s * |01> - s * |10>
      -- coeffs: |00>=0, |01>=s, |10>=-s, |11>=0
      psi = [0, s, -s, 0]
  in [[psi !! i * conjugate (psi !! j) | j <- [0..3]] | i <- [0..3]]

-- | Reduced density matrix of the singlet state for subsystem A.
singletReducedA :: DensityOp
singletReducedA = partialTraceB singletState

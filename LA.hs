module LA where

import Types
import Utils
import Data.List
import Data.Complex

-- Calculates the matTranspose of a matrix
matTranspose :: Matrix -> Matrix
matTranspose (m : ms)
  | (m : ms) == [] = []
  | ms == []       = map (\x -> [x]) (m)
  | otherwise      = zipWith (:) (m) (matTranspose ms)

-- Multiplies two matrices
matrixMul :: Matrix -> Matrix -> Matrix
matrixMul m1 m2
  | not (neighbouringDimensionsMatch m1 m2) = error "Matrix dimensions do not match. No. of columns of first matrix must be equal to no. of rows of second matrix"
  | otherwise = [[sum $ zipWith (*) row col | col <- matTranspose m2] | row <- m1]

-- Adds two matrices
matrixAdd :: Matrix -> Matrix -> Matrix
matrixAdd m1 m2
  | not (dimensionsMatch m1 m2) = error "Dimensions do not match"
  | otherwise = zipWith (zipWith (+)) m1 m2

-- Subtracts the second matrix from the first matrix
matrixSub :: Matrix -> Matrix -> Matrix
matrixSub m1 m2
  | not (dimensionsMatch m1 m2) = error "Dimensions do not match"
  | otherwise = zipWith (zipWith (-)) m1 m2


scalarMul :: Double -> Matrix -> Matrix
scalarMul s m  
  = map (map (*s)) m

determinant :: Matrix -> Double
determinant m
  | not (isSquare m)  = error "Matrix is not square"
  | length m == 1     = head (head m)
  | length m == 2     = determinant2 m
  | length m == 3     = determinant3 m
  | otherwise         = gaussDeterminant m
  where
    minor :: Matrix -> Int -> Int -> Matrix
    minor m i j
      = map (drop j) (take (i - 1) m ++ drop i m)

-- Solves a system of linear equations (Ax = b) using Gaussian elimination
gauss :: Matrix -> Vector -> Vector
gauss m v 
  = resubstitute $ triangulate m'
  where
    v' = map (\x -> [x]) v
    m' = zipWith (++) m v'

-- Gets the inverse of a matrix
inverse :: Matrix -> Matrix
inverse m
  | not (isSquare m) = error "Matrix is not square"
  | d == 0           = error "Matrix is not invertible"
  | otherwise        = matTranspose cs
  where
    d = determinant m
    -- Use the identity and column functions to generate a list of each of the column vectors of the identity matrix
    is  = map (column (identity $ length m)) [1..(length m)]
    -- Use the gauss function to solve the system of linear equations for each column vector
    cs  = map (gauss m) is

-- Determines if a set of vectors are linearly independent
linearlyIndependent :: [Vector] -> Bool
linearlyIndependent vs
  | length vs == 0 = True
  | otherwise      = determinant (matTranspose vs) /= 0

-- Determines if a set of vectors are linearly dependent
linearlyDependent 
  = not . linearlyIndependent

-- Gives a generating set for a subspace
-- Uses the combination function to generate all possible combinations of vectors
generatingSet :: [Vector] -> [Vector]
generatingSet vs
  = head $ filter linearlyIndependent $ combinations vs

-- Returns the kernel/null space of a matrix
kernel :: Matrix -> [Vector]
kernel m
  = generatingSet $ map (column m) [1..(length m)]

-- Returns the range of a matrix
range :: Matrix -> [Vector]
range m
  = generatingSet $ matTranspose m

-- Returns the dot product of two vectors
dot :: Vector -> Vector -> Double
dot v1 v2
  = sum $ zipWith (*) v1 v2

-- Returns the euclidean norm of a vector
norm :: Vector -> Double
norm v
  = sqrt $ dot v v


module Utils where

import Types
import Data.List

-- Checks if the number of columns of the first matrix is equal to the number of rows of the second matrix
neighbouringDimensionsMatch :: Matrix -> Matrix -> Bool
neighbouringDimensionsMatch m1 m2 
  = (length (head m1)) == (length m2)

-- Checks if the dimensions of the two matrices are the same
dimensionsMatch :: Matrix -> Matrix -> Bool
dimensionsMatch m1 m2 
  = (length m1) == (length m2) && (length (head m1)) == (length (head m2))

-- Checks if the matrix is a square matrix
isSquare :: Matrix -> Bool
isSquare m 
  | length m == 0 = True
  | otherwise     = (length m) == (length (head m))


-- Calculates the determinant of a 2x2 matrix
determinant2 :: Matrix -> Double
determinant2 m
  | isSquare m && length m == 2 = (a * d) - (b * c)
  | otherwise                   = error "Matrix is not 2x2"
  where 
    a : b : _ = head m
    c : d : _ = last m

-- Calculates the determinant of a 3x3 matrix
determinant3 :: Matrix -> Double
determinant3 n@(m : m' : ms : _)
  | isSquare n && length n == 3 = (a * e * i) + (b * f * g) + (c * d * h) - (c * e * g) - (b * d * i) - (a * f * h)
  | otherwise                   = error "Matrix is not 3x3"
  where
    a : b : c : _ = m
    d : e : f : _ = m'
    g : h : i : _ = ms

-- A function that replaces a row in a matrix with a given row (1-indexed)
replaceRow :: Matrix -> Int -> [Double] -> Matrix
replaceRow m i r
  | i < 1 || i > length m = error "Index out of bounds"
  | otherwise             = (take (i - 1) m) ++ [r] ++ (drop i m)


-- A function that swaps two rows in a matrix (1-indexed)
swapRows :: Matrix -> Int -> Int -> Matrix
swapRows m i j
  | i < 1 || i > length m || j < 1 || j > length m = error "Index out of bounds"
  | otherwise                                      = replaceRow (replaceRow m i (m !! (j - 1))) j (m !! (i - 1))

-- A function that multiplies a row in a matrix by a scalar (1-indexed)
rowMul :: Matrix -> Int -> Double -> Matrix
rowMul m i s
  | i < 1 || i > length m = error "Index out of bounds"
  | otherwise             = replaceRow m i (map (*s) (m !! (i - 1)))

-- Adds a multiple of one row to another row (1-indexed)
-- rowAdd m i s j adds s times the ith row to the jth row (1-indexed)
rowAdd :: Matrix -> Int -> Double -> Int -> Matrix
rowAdd m i s j
  | i < 1 || i > length m || j < 1 || j > length m = error "Index out of bounds"
  | otherwise                                      = replaceRow m j (zipWith (+) (m !! (j - 1)) (map (*s) (m !! (i - 1))))


-- Checks if a matrix has a zero row
hasZeroRow :: Matrix -> Bool
hasZeroRow [] = False
hasZeroRow (m : ms)
  = foldl (||) False (map (== zeros) (m : ms))
  where 
    zeros = replicate (length m) 0

-- Checks if a matrix has a non-zero row
hasNonZeroRow :: Matrix -> Bool
hasNonZeroRow [] = False
hasNonZeroRow (m : ms)
  = foldl (||) False (map (/= zeros) (m : ms))
  where 
    zeros = replicate (length m) 0


-- Checks that all zero rows are below all non-zero rows
-- Find the first zero row, and check that there are no non-zero rows below it
-- Uses the hasZeroRow and hasNonZeroRow functions
zeroRowsBelow :: Matrix -> Bool
zeroRowsBelow [] = True
zeroRowsBelow (m : ms)
  | not (hasZeroRow (m : ms)) = True
  | m == zeros                = not (hasNonZeroRow ms) && zeroRowsBelow ms
  | otherwise                 = zeroRowsBelow ms
  where 
    zeros = replicate (length m) 0

-- Gets the index of the leading entry of a row (1-indexed)
leadingEntry :: [Double] -> Int
leadingEntry [] = 0
leadingEntry (m : ms)
  | m /= 0    = 1
  | otherwise = 1 + leadingEntry ms


-- Removes all zero rows from a matrix
removeZeroRows :: Matrix -> Matrix
removeZeroRows [] = []
removeZeroRows (m : ms)
  | m == zeros = removeZeroRows ms
  | otherwise  = m : removeZeroRows ms
  where 
    zeros = replicate (length m) 0

-- Checks if a matrix is in row echelon form
-- A matrix is in row echelon form if:
-- 1. All nonzero rows are above any zero rows
-- 2. The leading entry of each row is to the right of the leading entry of the row above it
-- Uses the leadingEntry and zeroRowsBelow functions
-- Note: This function does not check that the leading entry of each row is 1
-- Uses the leadingEntry function to check that the leading entry of each row is to the right of the leading entry of the row above it

isRef :: Matrix -> Bool
isRef [] = True
isRef (_ : []) = True
isRef m
  | not (zeroRowsBelow m)        = False
  | otherwise                    = leadingEntry n < leadingEntry (head ns) && isRef ns
  where
    (n : ns) = removeZeroRows m

-- Finds the index of the first non-zero row in a matrix (1-indexed)
-- Uses the leadingEntry function
findNonZeroRow :: Matrix -> Int
findNonZeroRow [] = 0
findNonZeroRow (m : ms)
  | leadingEntry m == 0 = 1 + findNonZeroRow ms
  | otherwise           = 1

rotateLeadingEntry :: Matrix -> Matrix
rotateLeadingEntry (r : rs)
  | (head r) /= 0 = (r : rs)
  | otherwise     = rotateLeadingEntry (rs ++ [r])

resubstitute' :: Matrix -> Vector
resubstitute' [] = []
resubstitute' (r : rs)
  = a : (resubstitute' rs')
  where
    a = (head r) / (last r)
    rs' = map s' rs
    s' (a1 : (a2 : as')) = ((a1 - a * a2) : as')

resubstitute :: Matrix -> Vector
resubstitute = reverse . resubstitute' . reverse . map reverse

triangulate :: Matrix -> Matrix
triangulate [] = []
triangulate m 
  | m == zeros = []
  | otherwise = row : (triangulate rows')
  where
    zeros = replicate (length m) (replicate (length m) 0)
    (row : rows) = rotateLeadingEntry m
    rows' = map f rows
    f :: Row -> Row
    f xs 
      | xs == [] = []
      | head xs == 0 = if (length xs) > 1 then drop 1 xs else []
      | otherwise    = drop 1 $ zipWith (-) (xs) (map (* y) row)
      where
        y :: Double
        y = head xs / head row

-- Calculates the sign of the determinant of a matrix
signDeterminant :: Matrix -> Double
signDeterminant m 
  | even swaps = 1
  | otherwise  = -1
  where
    swaps = length $ filter (== True) $ zipWith (/=) (head m) (head $ triangulate m)

-- Calculates the determinant of a matrix using row reduction
gaussDeterminant :: Matrix -> Double
gaussDeterminant m 
  | length t /= length m = 0
  | otherwise            = signDeterminant m * (product $ map head t)
  where
    t = triangulate m


-- Returns the n x n identity matrix
identity :: Int -> Matrix
identity n
  = map (\i -> [if j == i then 1 else 0 | j <- [1..n]]) [1..n]

-- Joins two matrices together in a row-by-row fashion
join :: Matrix -> Matrix -> Matrix
join m1 m2
  = zipWith (++) m1 m2

-- Gives the nth column of a matrix (1-indexed)
column :: Matrix -> Int -> Row
column m n
  | n < 1 || n > length (head m) = error "Index out of bounds"
  | otherwise                    = map (!! (n - 1)) m

-- Returns all combinations of a list of lists
-- For example, combinations [[1,2],[3,4]] = [ [[1, 2]], [[3, 4]], [[1, 2], [3, 4]] ]
combinations :: [[a]] -> [[[a]]]
combinations [] 
  = []
combinations (l : ls)
  = [l] : (map (l :) (combinations ls)) ++ (combinations ls)

-- Returns the diagonal elements of a square matrix
diagonal :: Matrix -> Row
diagonal m
  | not (isSquare m) = error "Matrix is not square"
  | otherwise        = map (\i -> m !! (i - 1) !! (i - 1)) [1..length m]

-- Given a set of columnn vectors, returns the matrix formed by those vectors
-- For example, columns [[1,2],[3,4]] = [[1,3],[2,4]]
columnVectorsToMatrix :: [Vector] -> Matrix
columnVectorsToMatrix [] = []
columnVectorsToMatrix (v : vs)
  = zipWith (:) v (columnVectorsToMatrix vs)

-- Returns the trace of a matrix
trace :: Matrix -> Double
trace x
  | not (isSquare x) = error "Matrix is not square"
  | otherwise        = sum [sum [y | (n, y)<-zip [1..n] m, n==x]|(x, m)<-zip [1..n] x]
  where
    n = length x

module Complex where
import Types
import Utils
import Data.Complex



-- Transpose a matrix/list
transpose' a = foldr (zipWith(:)) (replicate (length a) []) a

-- Straight forward implementation for matrix-matrix multiplication
complexMatMul :: ComplexMatrix -> ComplexMatrix -> ComplexMatrix
complexMatMul m1 m2
  = [[sum $ zipWith (*) row col | col <- transpose' m2] | row <- m1]

-- Faddeev-LeVerrier algorithm for calculating the characteristic polynomial of a matrix
-- This is a recursive function that takes a matrix and a polynomial and returns the characteristic polynomial of the matrix
flv :: ComplexMatrix -> [Complex Double]
flv m = snd <$> scanl f (zeros, 1) [1..n]
  where 
    n = length m
    zeros = replicate n (replicate n 0)
    trace x = sum [sum [y | (n, y)<-zip [1..n] m, n==x] | (x, m)<-zip [1..n] x]
    diag d = [[sum [d | x == y] | y<-[1..n]] | x<-[1..n]]
    add ms ns = zipWith (zipWith (+)) ms ns
    f (u,d) k = (x, -trace (complexMatMul x m) / fromIntegral k)
      where x = add (diag d) (complexMatMul u m)

-- Laguerre method for finding roots of a polynomial, given an initial guess
laguerre :: [Complex Double] -> Complex Double -> Complex Double
laguerre p x 
  = if magnitude a < 1e-9 then x else laguerre p x'
  where 
    eval = foldl1 (\a b -> x * a + b)
    o' = length p - 1
    o  = fromIntegral $ length p - 1
    f' p = init $ zipWith (*) p $ map fromIntegral [o',o'-1..]
    g  = eval (f' p) / eval p
    h  = (g ** 2 - eval (f' (f' p)) / eval p)
    d  = sqrt $ (o-1) * (o*h - g**2)
    ga = g - d
    gb = g + d
    s = if magnitude ga < magnitude gb then gb else ga
    a = o / s
    x' = x - a

-- Computes roots of a polynomial 
roots :: [Complex Double] -> [Complex Double]
roots [a, b] = [-b / a]
roots p 
  = r : roots (removeRoot p)
  where 
    r = laguerre p 0
    removeRoot = init . scanl1 (\x y -> x * r + y)


-- Returns the eigenvalues of a matrix
eigenvalues :: ComplexMatrix -> [Complex Double]
eigenvalues m
    = roots $ flv m

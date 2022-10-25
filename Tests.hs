module Tests where

import LA
import Utils
import Complex
import TS.TestSuite


-- Test for the isSquare function
isSquareTestCases
  = [ ([[1, 1, 1], [1, 1, 1], [1, 1, 1]]) ==> True
    , ([[1, 1, 1], [1, 1, 1]]) ==> False
    , ([[1, 0], [0, 1]]) ==> True
    , ([[1, 0], [0, 1], [0, 0]]) ==> False
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> True
    , ([[1, 0, 0, 0, 0, 0, 0, 0, 0]]) ==> False
    , ([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]) ==> True
    ]

-- Test for the neighbouringDimensionsMatch function
neighbouringDimensionsMatchTestCases
  = [ ([[1, 1, 1], [1, 1, 1], [1, 1, 1]], [[1, 1, 1], [1, 1, 1], [1, 1, 1]]) ==> True
    , ([[1, 1, 1], [1, 1, 1], [1, 1, 1]], [[1], [1], [1]]) ==> True
    , ([[1, 1, 1], [1, 1, 1], [1, 1, 1]], [[1, 1, 1], [1, 1, 1]]) ==> False
    , ([[1, 0], [0, 1]], [[1, 0], [0, 1]]) ==> True
    , ([[1, 0], [0, 1]], [[1, 0], [0, 1], [0, 0]]) ==> False
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]], [[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> True
    , ([[1, 0, 0, 0, 0, 0, 0, 0, 0]], [[1, 0, 0, 0, 0, 0, 0, 0, 0]]) ==> False
    , ([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]], [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]) ==> True
    ]

-- Test for the dimensionsMatch function
dimensionsMatchTestCases
  = [ ([[1, 1, 1], [1, 1, 1], [1, 1, 1]], [[1, 1, 1], [1, 1, 1], [1, 1, 1]]) ==> True
    , ([[1, 1, 1], [1, 1, 1], [1, 1, 1]], [[1], [1], [1]]) ==> False
    , ([[1, 1, 1], [1, 1, 1], [1, 1, 1]], [[1, 1, 1], [1, 1, 1]]) ==> False
    , ([[1, 0], [0, 1]], [[1, 0], [0, 1]]) ==> True
    , ([[1, 0], [0, 1]], [[1, 0], [0, 1], [0, 0]]) ==> False
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]], [[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> True
    , ([[1, 0, 0, 0, 0, 0, 0, 0, 0]], [[1, 0, 0, 0, 0, 0, 0, 0, 0]]) ==> True
    , ([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]], [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]) ==> True
    ]

-- Test for the transpose function
matTransposeTestCases
  = [ ([[1, 1, 1], [1, 1, 1], [1, 1, 1]]) ==> [[1, 1, 1], [1, 1, 1], [1, 1, 1]]
    , ([[1, 1, 1], [1, 1, 1]]) ==> [[1, 1], [1, 1], [1, 1]]
    , ([[1, 0], [0, 1]]) ==> [[1, 0], [0, 1]]
    , ([[1, 0], [0, 1], [0, 0]]) ==> [[1, 0, 0], [0, 1, 0]]
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
    , ([[1, 0, 0, 0, 0, 0, 0, 0, 0]]) ==> [[1], [0], [0], [0], [0], [0], [0], [0], [0]]
    , ([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]) ==> [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
    ]

-- Test for the add function
matrixAddTestCases
  = [ ([[1, 1, 1], [1, 1, 1], [1, 1, 1]], [[1, 1, 1], [1, 1, 1], [1, 1, 1]]) ==> [[2, 2, 2], [2, 2, 2], [2, 2, 2]]
    , ([[1, 0], [0, 1]], [[1, 0], [0, 1]]) ==> [[2, 0], [0, 2]]
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]], [[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> [[2, 0, 0], [0, 2, 0], [0, 0, 2]]
    , ([[1, 0, 0, 0, 0, 0, 0, 0, 0]], [[1, 0, 0, 0, 0, 0, 0, 0, 0]]) ==> [[2, 0, 0, 0, 0, 0, 0, 0, 0]]
    ]

-- Test for the subtract function
matrixSubTestCases
  = [ ([[1, 1, 1], [1, 1, 1], [1, 1, 1]], [[1, 1, 1], [1, 1, 1], [1, 1, 1]]) ==> [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
    , ([[1, 0], [0, 1]], [[1, 0], [0, 1]]) ==> [[0, 0], [0, 0]]
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]], [[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
    , ([[1, 0, 0, 0, 0, 0, 0, 0, 0]], [[1, 0, 0, 0, 0, 0, 0, 0, 0]]) ==> [[0, 0, 0, 0, 0, 0, 0, 0, 0]]
    ]

-- Tests for the scalar multiplication function
scalarMulTestCases
  = [ (2, [[1, 1, 1], [1, 1, 1], [1, 1, 1]]) ==> [[2, 2, 2], [2, 2, 2], [2, 2, 2]]
    , (2, [[1, 0], [0, 1]]) ==> [[2, 0], [0, 2]]
    , (3.14159, [[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> [[3.14159, 0, 0], [0, 3.14159, 0], [0, 0, 3.14159]]
    , (2.90, [[1, 0, 0, 0, 0, 0, 0, 0, 0]]) ==> [[2.90, 0, 0, 0, 0, 0, 0, 0, 0]]
    ]

-- Tests for the replaceRow function
-- replaceRow m i r replaces the ith row of m with r
replaceRowTestCases
  = [ ([[1, 1, 1], [1, 1, 1], [1, 1, 1]], 1, [2, 2, 2]) ==> [[2, 2, 2], [1, 1, 1], [1, 1, 1]]
    , ([[1, 1, 1], [1, 1, 1], [1, 1, 1]], 2, [2, 2, 2]) ==> [[1, 1, 1], [2, 2, 2], [1, 1, 1]]
    , ([[1, 1, 1], [1, 1, 1], [1, 1, 1]], 3, [2, 2, 2]) ==> [[1, 1, 1], [1, 1, 1], [2, 2, 2]]
    , ([[1, 0], [0, 1]], 1, [2, 0]) ==> [[2, 0], [0, 1]]
    ]

-- Tests for the swapRows function
swapRowsTestCases
  = [ ([[1, 1, 1], [1, 1, 1], [1, 1, 1]], 1, 1) ==> [[1, 1, 1], [1, 1, 1], [1, 1, 1]]
    , ([[1, 0], [0, 1]], 1, 2) ==> [[0, 1], [1, 0]]
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]], 1, 2) ==> [[0, 1, 0], [1, 0, 0], [0, 0, 1]]
    ]

-- Tests for the rowMul function
rowMulTestCases
  = [ ([[1, 1, 1], [1, 1, 1], [1, 1, 1]], 1, 2) ==> [[2, 2, 2], [1, 1, 1], [1, 1, 1]]
    , ([[1, 0], [0, 1]], 1, 2) ==> [[2, 0], [0, 1]]
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]], 3, 3.14) ==> [[1, 0, 0], [0, 1, 0], [0, 0, 3.14]]
    ]

-- Tests for the rowAdd function
-- Adds a multiple of one row to another row (1-indexed)
-- rowAdd m i s j adds s times the ith row to the jth row (1-indexed)
rowAddTestCases
  = [ ([[1, 1, 1], [1, 1, 1], [1, 1, 1]], 1, 2, 2) ==> [[1, 1, 1], [3, 3, 3], [1, 1, 1]]
    , ([[1, 0], [0, 1]], 1, 2, 2) ==> [[1, 0], [2, 1]]
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]], 1, 3.14, 3) ==> [[1, 0, 0], [0, 1, 0], [3.14, 0, 1]]
    ]

-- Tests for the hasZeroRow function
hasZeroRowTestCases
  = [ ([[1, 1, 1], [1, 1, 1], [1, 1, 1]]) ==> False
    , ([[1, 0], [0, 1]]) ==> False
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> False
    , ([[1, 0, 0], [0, 0, 0], [0, 0, 1]]) ==> True
    , ([[0, 0, 0], [0, 0, 0], [0, 0, 0]]) ==> True
    ]

-- Tests for the hasNonZeroRow function
hasNonZeroRowTestCases
  = [ ([[1, 1, 1], [1, 1, 1], [1, 1, 1]]) ==> True
    , ([[1, 0], [0, 1]]) ==> True
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> True
    , ([[1, 0, 0], [0, 0, 0], [0, 0, 1]]) ==> True
    , ([[0, 0, 0], [0, 0, 0], [0, 0, 0]]) ==> False
    ]


-- Tests for the zeroRowsBelow function
-- Checks that all zero rows are below all non-zero rows
zeroRowsBelowTestCases
  = [ ([[1, 1, 1], [1, 1, 1], [1, 1, 1]]) ==> True
    , ([[1, 0], [0, 1]]) ==> True
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> True
    , ([[1, 0, 0], [0, 0, 0], [0, 0, 1]]) ==> False
    , ([[0, 0, 0], [0, 0, 0], [0, 0, 0]]) ==> True
    , ([[1, 0, 0], [0, 0, 0], [1, 0, 1]]) ==> False
    , ([[1, 0, 0], [0, 0, 0], [0, 0, 1]]) ==> False
    , ([[0, 0, 0], [0, 0, 0], [1, 0, 1]]) ==> False
    , ([[1, 0, 0], [0, 0, 0], [0, 0, 0]]) ==> True
    , ([[0, 0, 0], [1, 0, 0], [0, 0, 0]]) ==> False
    , ([[0, 0, 0], [0, 0, 0], [1, 0, 0]]) ==> False
    , ([[1, 1, 1], [0, 0, 0], [1, 1, 1]]) ==> False
    ]

-- Tests for the removeZeroRows function
-- Checks that all zero rows are removed
removeZeroRowsTestCases
  = [ ([[1, 1, 1], [1, 1, 1], [1, 1, 1]]) ==> [[1, 1, 1], [1, 1, 1], [1, 1, 1]]
    , ([[1, 0], [0, 1]]) ==> [[1, 0], [0, 1]]
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
    , ([[1, 0, 0], [0, 0, 0], [0, 0, 1]]) ==> [[1, 0, 0], [0, 0, 1]]
    , ([[0, 0, 0], [0, 0, 0], [0, 0, 0]]) ==> []
    , ([[1, 0, 0], [0, 0, 0], [1, 0, 1]]) ==> [[1, 0, 0], [1, 0, 1]]
    , ([[1, 0, 0], [0, 0, 0], [0, 0, 1]]) ==> [[1, 0, 0], [0, 0, 1]]
    , ([[0, 0, 0], [0, 0, 0], [1, 0, 1]]) ==> [[1, 0, 1]]
    , ([[1, 0, 0], [0, 0, 0], [0, 0, 0]]) ==> [[1, 0, 0]]
    ]



-- Tests for the isRef function
isRefTestCases
  = [ ([[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> True
    , ([[1, 3, 0, 0, 3], [0, 0, 1, 0, 9], [0, 0, 0, 1, -4]]) ==> True
    , ([[1, 0, 0], [1, 0, 0], [1, 1, 1]]) ==> False
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 0]]) ==> True
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 0], [0, 0, 0]]) ==> True
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 0], [0, 0, 1]]) ==> False
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 0], [0, 0, 0], [0, 0, 1]]) ==> False
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]]) ==> True
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 1]]) ==> False
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]]) ==> True
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 1]]) ==> False
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]]) ==> True
    ]

-- Tests for the gauss function
-- this function solves a system of linear equations Ax = b
gaussTestCases
  = [ ([[1, 1], [1, 2]], [2, 3]) ==> [1, 1]
    , ([[0, 0, 1], [1, 0, 0], [0, 1, 0]], [1, 1, 1]) ==> [1, 1, 1]
    ]

-- Tests for the determinant function 
determinantTestCases 
  = [ ([[1, 0], [0, 1]]) ==> 1
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> 1
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 0]]) ==> 0
    , ([[1, 2, 3], [4, 5, 6], [7, 8, 9]]) ==> 0
    , ([[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]) ==> 0
    , ([[42, 32, 55, 321], [31, 66, 23, 90], [43, 23, 43, 0], [1, 3, 2, 4]]) ==> 917833

    ]

inverseTestCases
  = [ [[1, 2, 3], [4, 5, 6], [7, 6, 6]] ==> [[2, -2, 1], [-6, 5, -2], [11/3, -8/3, 1]]
    , [[1, 2], [3, 4]] ==> [[-2, 1], [3/2, -1/2]]
    , [[1, 0, 0], [0, 1, 0], [0, 0, 1]] ==> [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
    ]

-- Tests for the linearlyIndependent function
linearlyIndependentTestCases
  = [ ([[1, 0], [0, 1]]) ==> True
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> True
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 0]]) ==> False
    , ([[1, 2, 3], [4, 5, 6], [7, 8, 9]]) ==> False
    , ([[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]) ==> False
    ]

-- Tests for the trace function
traceTestCases
  = [ ([[1, 0], [0, 1]]) ==> 2
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 1]]) ==> 3
    , ([[1, 0, 0], [0, 1, 0], [0, 0, 0]]) ==> 2
    , ([[1, 2, 3], [4, 5, 6], [7, 8, 9]]) ==> 15
    , ([[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]) ==> 34
    ]

-- Tests for the dot product function
dotTestCases 
  = [ ([1, 2, 3], [4, 5, 6]) ==> 32
    , ([1, 2, 3, 4], [5, 6, 7, 8]) ==> 70
    , ([1, 2, 3, 4, 5], [6, 7, 8, 9, 10]) ==> 130
    ]

-- Tests for the euclidean norm function
normTestCases 
  = [ ([1, 2, 3]) ==> sqrt 14
    , ([1, 2, 3, 4]) ==> sqrt 30
    , ([1, 2, 3, 4, 5]) ==> sqrt 55
    ]





linearAlgebraTestCases 
  = [ TestCase "isSquare" (isSquare) isSquareTestCases
    , TestCase "neighbouringDimensionsMatch" (uncurry neighbouringDimensionsMatch) neighbouringDimensionsMatchTestCases
    , TestCase "dimensionsMatch" (uncurry dimensionsMatch) dimensionsMatchTestCases
    , TestCase "transpose" (matTranspose) matTransposeTestCases
    , TestCase "matrixAdd" (uncurry matrixAdd) matrixAddTestCases
    , TestCase "matrixSub" (uncurry matrixSub) matrixSubTestCases
    , TestCase "scalarMul" (uncurry scalarMul) scalarMulTestCases
    , TestCase "replaceRow" (uncurry3 replaceRow) replaceRowTestCases
    , TestCase "swapRows" (uncurry3 swapRows) swapRowsTestCases
    , TestCase "rowMul" (uncurry3 rowMul) rowMulTestCases
    , TestCase "rowAdd" (uncurry4 rowAdd) rowAddTestCases
    , TestCase "hasZeroRow" (hasZeroRow) hasZeroRowTestCases
    , TestCase "hasNonZeroRow" (hasNonZeroRow) hasNonZeroRowTestCases
    , TestCase "zeroRowsBelow" (zeroRowsBelow) zeroRowsBelowTestCases
    , TestCase "isRef" (isRef) isRefTestCases
    , TestCase "gauss" (uncurry gauss) gaussTestCases
    , TestCase "determinant" (determinant) determinantTestCases
    , TestCase "inverse" (inverse) inverseTestCases
    , TestCase "linearlyIndependent" (linearlyIndependent) linearlyIndependentTestCases
    , TestCase "trace" (trace) traceTestCases
    , TestCase "dot" (uncurry dot) dotTestCases
    , TestCase "norm" (norm) normTestCases
    
    

    
    ]



main = mapM_ goTest linearAlgebraTestCases


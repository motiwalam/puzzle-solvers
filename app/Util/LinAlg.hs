{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Util.LinAlg where

import Data.List (findIndex, transpose)

class Field f where
    zero :: f
    add :: f -> f -> f
    neg :: f -> f

    one :: f
    mul :: f -> f -> f
    inv :: f -> f

    sub :: f -> f -> f
    sub x y = add x (neg y)

    div :: f -> f -> f
    div x y = mul x (inv y)


class (Field f) => VectorSpace v f | v -> f where
    vzero :: v
    vadd :: v -> v -> v
    smul :: f -> v -> v

    vneg :: v -> v
    vneg v = smul (neg one) v


data Z2 = Zero | One deriving (Eq, Show)
instance Field Z2 where
    zero = Zero

    add Zero Zero = Zero
    add One  One  = Zero
    add _    _    = One

    neg = id

    one = One

    mul One One = One
    mul _   _   = Zero

    inv = id

newtype Vec f = Vec [f] deriving (Eq, Show)
instance (Field f) => VectorSpace (Vec f) f where
    vzero = Vec (repeat zero)
    vadd (Vec xs) (Vec ys) = Vec (zipWith add xs ys)
    smul s (Vec xs) = Vec (map (mul s) xs)


-- find the coordinates of a vector in a given spanning set of vectors, if they exist
-- that is, given a spanning set {b1, b2, ..., bn} and a vector v, find scalars c1, c2, ... cn such that
-- v = (smul c1 b1) `vadd` (smul c2 b2) `vadd` ... (smul cn bn)
-- or Nothing if no such scalars exist (i.e. if v is not in the span of the bi)
-- note that the bi need not be linearly independent, so there may be multiple valid sets of coordinates; this function can return any one of them
-- this algorithm assumes a coordinate system shared by the spanning vectors and the target vector given by the components function argument
computeCoordinates :: (VectorSpace v f, Eq f) => (v -> [f]) -> [v] -> v -> Either String [f]
computeCoordinates components basisVecs target = extractSolution n rref pivotCols
  where
    n = length basisVecs
    -- augmented matrix: row j = [b1[j], b2[j], ..., bn[j], v[j]]
    (rref, pivotCols) = toRREF n $ transpose (map components basisVecs ++ [components target])

-- reduce an augmented matrix to RREF, pivoting only among the first nVars columns.
-- returns (reduced matrix, pivot column indices in order).
toRREF :: (Field f, Eq f) => Int -> [[f]] -> ([[f]], [Int])
toRREF nVars mat = go mat 0 0 []
  where
    nRows = length mat

    go m row col pivots
      | row >= nRows || col >= nVars = (m, reverse pivots)
      | otherwise =
          case findIndex (\r -> r !! col /= zero) (drop row m) of
            Nothing  -> go m row (col + 1) pivots
            Just off ->
                let m1 = swapRows m row (row + off)
                    m2 = mapAt row (map (mul (inv (m1 !! row !! col)))) m1
                    m3 = foldl (elimCol row col) m2 [0 .. nRows - 1]
                in go m3 (row + 1) (col + 1) (col : pivots)

    swapRows m i j
      | i == j    = m
      | otherwise = [if k == i then m !! j else if k == j then m !! i else m !! k | k <- [0 .. length m - 1]]

    mapAt i f xs = take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs

    -- subtract a multiple of the pivot row from row i to zero out column col
    elimCol pivotRow col m i
      | i == pivotRow        = m
      | m !! i !! col == zero = m
      | otherwise =
          let factor = m !! i !! col
          in mapAt i (zipWith (\p r -> sub r (mul factor p)) (m !! pivotRow)) m

-- given the RREF matrix and pivot columns, check consistency and read off a solution.
-- free variables (non-pivot columns) are assigned zero.
extractSolution :: (Field f, Eq f) => Int -> [[f]] -> [Int] -> Either String [f]
extractSolution n mat pivotCols
  | any inconsistentRow mat = Left "Inconsistent system"
  | otherwise               = Right $ map resolve [0 .. n - 1]
  where
    inconsistentRow row = all (== zero) (take n row) && last row /= zero
    pivotMap            = zip pivotCols [0 ..]
    resolve j           = maybe zero (\row -> last (mat !! row)) (lookup j pivotMap)

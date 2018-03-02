module Matrix

import Data.Vect

Matrix : Nat -> Nat -> Type -> Type
Matrix n m t = Vect n (Vect m t)

xs : Matrix 3 2 Int
xs = [ [1, 2],
       [3, 4],
       [5, 6] ]

neg : Int -> Int
neg x = -x

negMatrix : Matrix n m Int -> Matrix n m Int
negMatrix = map (map neg)

rowSumOfMatrix : Matrix n m Int -> Vect n Int
rowSumOfMatrix = map sum

xsNeg : Matrix 3 2 Int
xsNeg = negMatrix xs

xsSum : Vect 3 Int
xsSum = rowSumOfMatrix xs

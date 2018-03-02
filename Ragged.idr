module Ragged

import Data.Vect

neg : Int -> Int
neg x = -x

-- This models a ragged matrix using a vector which holds the length of each row.
-- The matrix elements are dependent pairs, where the first component is the
-- index of the row used to index into the length vector.
-- The Fin enforces that the indices into the length vector are in bound.
-- The order of the rows is not enforced by the type, e.g. it is even fine to
-- index into the same elemt multiple times.
RaggedMatrix : (n: Nat) -> Vect n Nat -> Type -> Type
RaggedMatrix n ms t = Vect n (i: (Fin n) ** Vect (Data.Vect.index i ms) t)

RowLength : Vect 3 Nat
RowLength = [2, 1, 4]

xs : RaggedMatrix 3 RowLength Int
xs = [ (0 ** [1, 2]),
       (1 ** [3]),
       (2 ** [4, 5, 6, 7]) ]

negRaggedMatrix : RaggedMatrix n ms Int -> RaggedMatrix n ms Int
negRaggedMatrix = map ( \ (_ ** x) => (_ ** map neg x) )

rowSumOfRaggedMatrix : RaggedMatrix n ms Int -> Vect n Int
rowSumOfRaggedMatrix = map (\ (_ ** x) => sum x )

xsNeg : RaggedMatrix 3 RowLength Int
xsNeg = negRaggedMatrix xs

xsSum : Vect 3 Int
xsSum = rowSumOfRaggedMatrix xs

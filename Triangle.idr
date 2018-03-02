module Triangle

import Data.Vect

neg : Int -> Int
neg x = -x

-- This models a triangular matrix using a dependent pair, where the first
-- component is an arbitrary Nat corresponding to the length of the row.
TriangularMatrix : Nat -> Type -> Type
TriangularMatrix n t = Vect n (m: Nat ** Vect m t)

-- Using this design we can leave the index blank as it is infered from the type
xs : TriangularMatrix 3 Int
xs = [ (_ ** [1]),
       (_ ** [2, 3]),
       (_ ** [4, 5, 6]) ]
{-
-- This models a triangular matrix using a dependent pair, where the first
-- component is a number smaller than n (see Data.Fin) and the length of the
-- row is then (n + 1).
TriangularMatrix : Nat -> Type -> Type
TriangularMatrix n t = Vect n (m: (Fin n) ** Vect ((finToNat m) + 1) t)

xs : TriangularMatrix 3 Int
xs = [ (0 ** [1]),
       (1 ** [2, 3]),
       (2 ** [4, 5, 6]) ]
-}

negTriangularMatrix : TriangularMatrix n Int -> TriangularMatrix n Int
negTriangularMatrix = map ( \ (_ ** x) => (_ ** map neg x) )
-- or:
{-
negTriangularMatrix = map (dmap (map neg)) where
  -- Can't get the generic version to work.
  -- This definition type checks, but it's application doesn't
  -- dmap : {a : Type} -> {P : a -> Type} ->
  --        ({b: Type} -> b -> b) -> DPair a P -> DPair a P
  dmap : ({n: Nat} -> Vect n t -> Vect n t) -> (m: Nat ** Vect m t) -> (m: Nat ** Vect m t)
  dmap f (x ** y) = (x ** (f y))
-}

rowSumOfTriangularMatrix : TriangularMatrix n Int -> Vect n Int
rowSumOfTriangularMatrix = map (\ (_ ** x) => sum x )

xsNeg : TriangularMatrix 3 Int
xsNeg = negTriangularMatrix xs

xsSum : Vect 3 Int
xsSum = rowSumOfTriangularMatrix xs

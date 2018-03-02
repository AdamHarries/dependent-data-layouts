module GeneralisedDependentMatrix

import Data.Vect

neg : Int -> Int
neg x = -x

-- A generalisation of the TriangularMatrix type
DependentMatrix : (n: Nat) -> (Fin n -> Nat) -> Type -> Type
DependentMatrix n f t = Vect n (m: (Fin n) ** Vect (f m) t)

-- Triangular ascending row length
AscendingMatrix : Nat -> Type -> Type
AscendingMatrix n t = DependentMatrix n (\m => (finToNat m) + 1) t

xs : AscendingMatrix 3 Int
xs = [ (0 ** [1]),
       (1 ** [2, 3]),
       (2 ** [4, 5, 6]) ]

xsNeg : AscendingMatrix 3 Int
xsNeg = map (\ (_ ** x) => (_ ** map neg x)) xs

xsSum : Vect 3 Int
xsSum = map (\ (_ ** x) => sum x) xs

-- Triangular descending row length
DescendingMatrix : Nat -> Type -> Type
DescendingMatrix n t = DependentMatrix n (\m => minus n (finToNat m)) t

ys : DescendingMatrix 3 Int
ys = [ (0 ** [1, 2, 3]),
       (1 ** [4, 5]),
       (2 ** [6]) ]

ysNeg : DescendingMatrix 3 Int
ysNeg = map (\ (_ ** y) => (_ ** map neg y)) ys

ysSum : Vect 3 Int
ysSum = map (\ (_ ** y) => sum y) ys

-- Binary Tree
BinaryTree : Nat -> Type -> Type
BinaryTree n t = DependentMatrix n (\m => power 2 (finToNat m)) t

zs : BinaryTree 3 Int
zs = [ (0 ** [     1    ]),
       (1 ** [  2,    3 ]),
       (2 ** [4, 5, 6, 7]) ]

zsNeg : BinaryTree 3 Int
zsNeg = map (\ (_ ** z) => (_ ** map neg z)) zs

zsSum : Vect 3 Int
zsSum = map (\ (_ ** z) => sum z) zs

module Homology where

import SNF (smithNormalForm)
import Matrix ( Matrix, mbounds )
import Text.Printf ( printf )


type ZModule = [Integer]
type ZChain = [Integer]

{-
    ℤ^n -A-> ℤ^m -B-> ℤ^k is part of a chain complex (i.e. BA = 0)
    the homology module is the direct sum of the torsion and free parts of Ker[B]/Im[A]
    this is represented as a list of integers, where there the positive integers are the orders of the torsion parts
    and the zero integers each represent a free part, so that for example [2, 3, 0, 0] represents
    ℤ/2ℤ ⊕ ℤ/3ℤ ⊕ ℤ ⊕ ℤ
-}
homologyModule :: Matrix Integer -> Matrix Integer -> ZModule
homologyModule a b = ans where
    (na, ma) = mbounds a
    (nb ,mb) = mbounds b

    smithA = smithNormalForm a
    smithB = smithNormalForm b
    torsion = filter (>= 1) smithA
    bNullity = mb - length (filter (/= 0) smithB)
    free = replicate (bNullity - length torsion) 0

    ans
      | na == 0 || ma == 0                                                       = replicate bNullity 0
      | nb == 0 || mb == 0                                                       = filter (/= 1) $ torsion ++ replicate (na - length torsion) 0
      | mb /= na                                                                 = error $ printf "Incompatible matrix dimensions of A (%d, %d) and B (%d, %d) to compute the product BA \n A : %s \n B : %s" na ma nb mb (show a) (show b)
      | otherwise                                                                = filter (/= 1) $ torsion ++ free


homologyTorsion :: Matrix Integer -> Matrix Integer -> [Integer]
homologyTorsion a b = filter (/= 0) $ homologyModule a b

homologyFreeRank :: Matrix Integer -> Matrix Integer -> Int
homologyFreeRank a b = length $ filter (== 0) $ homologyModule a b

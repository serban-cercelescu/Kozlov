module Matrix (
    Matrix(..),
    mbounds,
    transpose,
    matToList,
    matToString
) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Array ( Array, array, (!), bounds )
import Data.List (intercalate)

data Matrix a = Matrix (Int, Int) [(Int, Int, a)]
    deriving (Show, Eq)

mbounds :: Matrix a -> (Int, Int)
mbounds (Matrix (n, m) _) = (n, m)

matToList :: (Num a) => Matrix a -> [[a]]
matToList (Matrix (n, m) mx) = ans where
    dict = Map.fromList [((i, j), x) | (i, j, x) <- mx]
    ans = [[Map.findWithDefault 0 (i, j) dict | j <- [1 .. m]] | i <- [1 .. n]]

transpose :: Matrix a -> Matrix a
transpose (Matrix (n, m) mx) = Matrix (m, n) $ do
    (i, j, x) <- mx
    return (j, i, x)

matToString :: (Show a, Num a) => Matrix a -> String
matToString matrix = intercalate " " $ fmap show $ concat $ matToList matrix

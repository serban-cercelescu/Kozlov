module Graph where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (sort)

import Control.Monad (forM_)
import Control.Monad.Writer (execWriter, tell)

import Simplicial

data Graph = Graph Int (Map Int [Int])

instance Show Graph where
    show (Graph n edges) = execWriter $ do
        tell $ "Vertices: " ++ show n ++ "\n"
        forM_ (Map.toList edges) $ \(i, js) -> do
            forM_ js $ \j -> do
                tell $ show i ++ " -> " ++ show j ++ "\n"


newGraph :: Int -> [(Int, Int)] -> Graph
newGraph n edges = Graph n ans where
    unsorted = foldr (\(a, b) -> Map.insertWith (++) a [b]) Map.empty edges
    ans = Map.fromAscList $ (\i -> (i, sort $ unsorted ! i)) <$> [1 .. n]

newUndirected :: Int -> [(Int, Int)] -> Graph
newUndirected n edges = newGraph n (edges ++ [(b, a) | (a, b) <- edges])

graphProduct :: Graph -> Graph -> Graph
graphProduct (Graph n1 e1) (Graph n2 e2) = Graph n ans where
    n = n1 * n2
    toProd i j = (i - 1) * n1 + j
    ans = Map.fromAscList $ do
        i <- [1 .. n1]
        j <- [1 .. n2]
        let ei = Map.findWithDefault [] i e1
        let ej = Map.findWithDefault [] j e2
        return (toProd i j, nubOrd [toProd a b | a <- ei, b <- ej])


morphismsList :: Graph -> Graph -> [[(Int, Int)]]
morphismsList g1 g2 = bkt (Map.empty) 1 where
    n1 = numVertices g1
    n2 = numVertices g2

    numVertices :: Graph -> Int
    numVertices (Graph n _) = n

    edges :: Graph -> Map Int [Int]
    edges (Graph _ e) = e

    isG2Edge :: Int -> Int -> Bool
    isG2Edge i j = case Map.lookup i (edges g2) of
        Nothing -> False
        Just js -> j `elem` js

    bkt :: Map Int Int -> Int -> [[(Int, Int)]]
    bkt m i
        | i > n1 = [Map.toList m]
        | otherwise = do
            j <- [1 .. n2]
            let ei = Map.findWithDefault [] i (edges g1)
            let ej = Map.findWithDefault [] j (edges g2)
            let m' = Map.insert i j m

            if and [Map.lookup a m' == Nothing || isG2Edge j (m' ! a) | a <- ei]  
                then bkt m' (i + 1)
                else []


hasMorphism :: Graph -> Graph -> Bool
hasMorphism g1 g2 = null $ morphismsList g1 g2 -- lazyness ftw


toBoxComplex :: Graph -> SimplicialComplex
toBoxComplex (Graph n edges) = ans where
    ans = newSimplicialComplex m simplices
    simplices0 = bkt vs []
    simplices = ans0 where
        vmap = Map.fromList $ zip vs [1 ..]
        ans0 = fmap (fmap (vmap !)) simplices0

    vs = do
        i <- [1 .. n]
        j <- Map.findWithDefault [] i edges
        return (i, j)
    m = length vs

    edgeSet = Set.fromList vs

    bkt :: [(Int, Int)] -> [(Int, Int)] -> [[(Int, Int)]]
    bkt [] x = [x]
    bkt ((i, j) : rem) acc = (if check (i, j) acc then bkt rem ((i, j) : acc) else []) ++ bkt rem acc

    check :: (Int, Int) -> [(Int, Int)] -> Bool
    check (i, j) acc = and [Set.member (i, rm) edgeSet | rm <- r] && and [Set.member (lm, j) edgeSet | lm <- l] where
        l = Set.toList $ Set.fromList (fmap fst acc)
        r = Set.toList $ Set.fromList (fmap snd acc)


clique :: Int -> Graph
clique n = newGraph n [(i, j) | i <- [1 .. n], j <- [1 .. n], i /= j]


boxHomologySequence :: Graph -> [[Integer]]
boxHomologySequence = homologySequence . toBoxComplex


homologyProductPreserving :: Graph -> Maybe Int
homologyProductPreserving g
    | (head gHomSeq == [0]) && length (filter (/= []) gHomSeq) == 2 = Just $ fst $ head $ filter ((/= []) . snd) $ tail $ zip [0..] gHomSeq
    | otherwise = Nothing
    where gHomSeq = boxHomologySequence g

------ TOOLS ------

nubOrd :: Ord a => [a] -> [a]
nubOrd = Set.toList . Set.fromList
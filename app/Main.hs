module Main (main) where

import Control.Monad
import Simplicial
import Graph

main :: IO ()
main = do
    let hs = homologySequence $ toBoxComplex $ clique 6
    forM_ hs $ \h -> do
        print h
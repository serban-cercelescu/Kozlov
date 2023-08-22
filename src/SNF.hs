{-# LANGUAGE ForeignFunctionInterface #-}
module SNF (smithNormalForm) where

import Data.Array ((!))
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array

import Matrix
import Foreign.Storable

foreign import ccall "csmith_normal_form" cSmithNormalForm :: Ptr CLLong -> IO (Ptr CLLong)

smithNormalForm :: Matrix Integer -> [Integer]
smithNormalForm (Matrix (n, m) matrix) = unsafePerformIO $ do
    let noEntries = length matrix
    cinput <- mallocArray (8 * (noEntries + 3)) :: IO (Ptr CLLong)
    pokeArray cinput
        (fromIntegral n : fromIntegral m : fromIntegral noEntries : concat [fmap fromIntegral [i, j] ++ [fromIntegral val] | (i, j, val) <- matrix])
    coutput <- cSmithNormalForm cinput
    k <- peek coutput

    map fromIntegral <$> (peekArray (fromIntegral k) (plusPtr coutput 8) :: IO [CLLong])

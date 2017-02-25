module Main where

import Control.Monad
import Criterion.Main
import Data.ByteString.Lazy (ByteString, pack)
import Data.Memorable
import Data.Memorable.Theme.Words
import System.Random
import Data.Word

benchmarks :: Word8 -> Word32 -> Word64 -> [Benchmark]
benchmarks w8 w32 w64 =
    [ bgroup "words"
        [ bench "word8"               $ nf (renderMemorable words8) w8
        , bench "threeWordsFor32Bits" $ nf (renderMemorable threeWordsFor32Bits) w32
        , bench "fourWordsFor32Bits"  $ nf (renderMemorable fourWordsFor32Bits) w32
        , bench "sixWordsFor64Bits"   $ nf (renderMemorable sixWordsFor64Bits) w64
        , bench "eightWordsFor64Bits" $ nf (renderMemorable eightWordsFor64Bits) w64
        , bench "big tuple"           $ nf (renderMemorable (four eightWordsFor64Bits)) (w64,w64,w64,w64)
        ]
    ]

main :: IO ()
main = do
    w8 <- randomIO
    w32 <- randomIO
    w64 <- randomIO
    defaultMain $ benchmarks w8 w32 w64

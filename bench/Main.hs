{-# OPTIONS_GHC -fno-warn-tabs #-}
module Main where

import Control.Monad
import Criterion.Main
import Data.ByteString.Lazy (ByteString, pack)
import Data.Memorable
import Data.Memorable.Theme.Words
import Data.Memorable.Theme.Fantasy
import Data.Memorable.Theme.Science
import System.Random

benchmarks :: ByteString -> [Benchmark]
benchmarks d =
	[ bgroup "words"
		[ bench "oneWord"    $ nf (renderMemorable oneWord) d
		, bench "twoWords"   $ nf (renderMemorable twoWords) d
		, bench "threeWords" $ nf (renderMemorable threeWords) d
		, bench "fourWords"  $ nf (renderMemorable fourWords) d
		, bench "rpgThings"  $ nf (renderMemorable rpgThings) d
		, bench "chemBabble"  $ nf (renderMemorable chemBabble) d
		]
	]

main :: IO ()
main = do
	d <- pack <$> replicateM 500 randomIO
	defaultMain $ benchmarks d

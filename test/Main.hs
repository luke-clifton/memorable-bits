{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Memorable
import Data.Memorable.Theme.Words
import Data.List
import GHC.TypeLits
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Test"
	[ properties
	, unitTests
	]

properties :: TestTree
properties = testGroup "Properties"
	[ 
	]

scProps = testGroup "(checked by SmallCheck)"
	[ ]

uniqnessTest :: (KnownNat (MinBits a), MemRender a, ConstantSize a) => Proxy a -> Assertion
uniqnessTest = assertEqual "" 1 . maximum . map length . group . sort . renderAll

unitTests :: TestTree
unitTests = testGroup "Unit tests"
	[ testCase "All unique oneWord" $ uniqnessTest oneWord
	, testCase "All unique twoWords" $ uniqnessTest (Proxy :: Proxy (WordsBits 4 :<> WordsBits 4))
        , testCase "All unique numbers (Dec)" $ uniqnessTest (Proxy :: Proxy (Number Dec 5))
        , testCase "All unique numbers (Hex)" $ uniqnessTest (Proxy :: Proxy (Number Hex 5))
	]

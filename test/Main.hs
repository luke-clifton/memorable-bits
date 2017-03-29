{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Memorable.Internal
import Data.Proxy
import Data.Memorable.Theme.Words
import Data.Word
import Data.Int
import Data.List
import GHC.TypeLits
import Text.Printf
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit
import Test.DocTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Test"
    [ properties
    , unitTests
    ]

type TestType = (Int8, Int32, Word8, Word64)
testPat = padHex (words8 .- words10)
testPat2 = padDec (hex8 .- words4 .- dec4)

properties :: TestTree
properties = testGroup "Properties"
    [ QC.testProperty "is show" (\x -> show (x :: Word8) == renderMemorable dec8 x)
    , QC.testProperty "is show tuple" (\(a,b) -> show (a :: Word8) ++ "-" ++ show (b :: Word8) == renderMemorable (two dec8) (a,b))
    , QC.testProperty "is %02x" (\x -> printf "%02x" (x :: Word8) == renderMemorable hex8 x)
    , QC.testProperty "is %04x" (\x -> printf "%04x" (x :: Word16) == renderMemorable hex16 x)
    , QC.testProperty "is %08x" (\x -> printf "%08x" (x :: Word32) == renderMemorable hex32 x)
    , QC.testProperty "tuples" (\(a,b) -> printf "%02x-%02x" (a :: Word8) (b :: Word8) == renderMemorable (hex8 .- hex8) (a,b))
    , QC.testProperty "render/parse" (\a -> runParser (runRender (a :: TestType)) == a)
    , QC.testProperty "rMem/pMem" (\a -> parseMemorable testPat (renderMemorable testPat a) == Just (a :: TestType))
    , QC.testProperty "rMem/pMem" (\a -> parseMemorable testPat2 (renderMemorable testPat2 a) == Just (a :: TestType))
    ]

scProps = testGroup "(checked by SmallCheck)"
    [
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "DocTest" $ doctest ["./src/"]
    , testGroup "Unique"
        [ testCase "padHex words4" $ assert (uniqueRenderings (padHex words4))
        , testCase "padDec words4" $ assert (uniqueRenderings (padDec words4))
        , testCase "words4 .- words4" $ assert (uniqueRenderings (words4 .- words4))
        , testCase "Hex" $ assert (uniqueRenderings (Proxy :: Proxy (Number Dec 8)))
        , testCase "Dec" $ assert (uniqueRenderings (Proxy :: Proxy (Number Hex 8)))
        ]
    ]

uniqueRenderings :: (Depth p ~ 8, MemRender p) => Proxy p -> Bool
uniqueRenderings p =
    let
        ls = map (renderMemorable p) [minBound .. maxBound :: Word8]
    in
        ls == nub ls

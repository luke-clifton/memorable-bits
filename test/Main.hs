{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
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
import Test.DocTest

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

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "DocTest" (doctest ["./src/"])
    ]

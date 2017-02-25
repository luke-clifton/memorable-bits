{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Data.Memorable.Theme.Science where

import Data.Memorable


type NumericPrefix = ToTree '["di", "tri", "4", "5", "9", "mono", "8", "hex"]
type ChemPrefix = ToTree '["propyl", "ethyl", "pyridine", "chloro", "methyl", "benzene", "hydro", "ferro"]
type ChemSuffix = ToTree '["oxide", "carbide", "sulfide", "fluoride"]
type ChemBabble = NumericPrefix :- ChemPrefix :- NumericPrefix :- ChemPrefix :- ChemSuffix

chemBabble :: Proxy ChemBabble
chemBabble = Proxy


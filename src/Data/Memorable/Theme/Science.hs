{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Data.Memorable.Theme.Science where

import Data.Memorable


type NumericPrefix a
	= ((a :<|> ("di" :<> a)) :<|> (("tri" :<> a) :<|> ("4" :<> "-" :<> a)))
	:<|> ((("6" :<> "-" :<> a):<|>("9" :<> "-" :<> a)) :<|> (("mono" :<> a):<|>("8" :<> "-" :<> a)))

type ChemPrefix = ToTree '["propyl", "ethyl", "pyridine", "sd", "methyl", "benzene", "hydro", "ferro"]
type ChemSuffix = ToTree '["oxide", "carbide", "sulfide", "fluoride"]
type ChemBabble = NumericPrefix ChemPrefix :<> "-" :<> NumericPrefix ChemPrefix :<> "-" :<> ChemSuffix

chemBabble :: Proxy ChemBabble
chemBabble = Proxy

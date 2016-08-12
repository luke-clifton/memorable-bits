{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Data.Memorable.Theme.Food where

import Data.Memorable

type DessertFlavours = ToTree
	'[ "strawberry"
	 , "chocolate"
	 , "vanilla"
	 , "blueberry"
	 , "raspberry"
	 , "apple"
	 , "almond"
	 , "cherry"
	 ]

type DessertTypes = ToTree
	'[ "custard"
	 , "parfait"
	 , "souffle"
	 , "pancakes"
	 , "icecream"
	 , "tart"
	 , "pie"
	 , "cupcakes"
	 ]

type FoodAdjectives = ToTree
	'[ "delicious"
	 , "rancid"
	 , "heavenly"
	 , "scrumptious"
	 , "delightful"
	 , "disgusting"
	 , "foul"
	 , "exotic"
	 , "bland"
	 , "gourmet"
	 , "tasty"
	 , "tasteless"
	 , "refreshing"
	 , "sensational"
	 , "crunchy"
	 , "creamy"
	 ]

type Desserts
    = FoodAdjectives :<-> DessertFlavours :<-> DessertTypes

desserts :: Proxy Desserts
desserts = Proxy

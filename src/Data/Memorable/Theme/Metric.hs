{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Data.Memorable.Theme.Metric where

import Data.Memorable

type UnitsPlural = ToTree
    '[ "meters"
     , "seconds"
     , "grams"
     , "liters"
     , "volts"
     , "ohms"
     , "amps"
     , "henries"
     ]

type Units = ToTree
    '[ "meter"
     , "second"
     , "gram"
     , "liter"
     , "volt"
     , "ohm"
     , "amp"
     , "henry"
     ]

type SIPrefix = ToTree
    '[ "atto"
     , "femto"
     , "pico"
     , "nano"
     , "micro"
     , "milli"
     , "centi"
     , "deci"
     , "deca"
     , "hecto"
     , "kilo"
     , "mega"
     , "giga"
     , "tera"
     , "peta"
     , "exa"
     ]

type Measurements
    = Number Dec 5
    :- "."
    :- Number Dec 3
    :- " "
    :- SIPrefix
    :- UnitsPlural
    :- " per "
    :- SIPrefix
    :- Units
    :- "^"
    :- NumberWithOffset Dec 2 2

measurements :: Proxy Measurements
measurements = Proxy

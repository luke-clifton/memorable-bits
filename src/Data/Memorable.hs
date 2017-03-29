module Data.Memorable
    ( renderMemorable
    , Memorable(..)
    -- * Pattern Building
    , (.-), (.|)
    , two, three, four, five
    , padHex, padDec
    , hex4, hex8, hex16, hex32, dec4, dec8, dec16, dec32
    , ToTree
    , leftSide
    , rightSide
    -- * Pattern Types
    , (:-)
    , MemRender()
    , Number
    , NumberWithOffset
    , PadTo
    , Dec
    , Hex
    , Depth
    , getDepth
    , LeftSide
    , RightSide
    -- * Re-export
    , Proxy(..)
    ) where

import Data.Memorable.Internal
import Data.Proxy

module Data.Memorable
    ( renderMemorable
    , Memorable(..)
    -- * Pattern Building
    , (.-), (.|)
    , two, three, four, five
    , padHex, padDec
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

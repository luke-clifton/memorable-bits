{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

-- | Words is special because GHC can't handle this many Symbols.
-- The `Choose` trick helps, but only to about 1024 words, after
-- which the compiler fails.
module Data.Memorable.Theme.Words where

import Data.Memorable
import Data.Memorable.Theme.Words.Internal
import Data.Vector
import GHC.TypeLits


-- | Allows you to select how many bits to query for this word.
-- Words are ordered by how common they are, and reducing bit
-- count selects from more common words first.
data WordsBits (n :: Nat)

-- | All the words.
type Words = WordsBits 12

type instance MaxBits (WordsBits n) = n
type instance MinBits (WordsBits n) = n
type instance Prune (WordsBits n) = WordsBits (n - 1)

instance (KnownNat n, n <= 12) => MemRender (WordsBits n) where
	render _ = do
		let
			bs = natVal (Proxy :: Proxy n)
		i <- getWord $ fromIntegral bs
		return $ words_list ! fromIntegral i


oneWord :: (ConstantSize Words, MemRender Words) => Proxy Words
oneWord = Proxy

twoWords :: Proxy (Words :<> "-" :<> Words)
twoWords = Proxy

threeWords :: Proxy (Words :<> "-" :<> Words :<> "-" :<> Words)
threeWords = Proxy

fourWords :: Proxy (Words :<> "-" :<> Words :<> "-" :<> Words :<> "-" :<> Words)
fourWords = Proxy

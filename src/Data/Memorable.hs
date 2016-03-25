{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Monad.State
import Data.Bits
import Data.Proxy
import Data.Word
import GHC.TypeLits
import Data.Binary.Get (runGet)
import Data.Binary.Bits.Get

data a :<|> b

data a :<> b

type family ToTreeH (a :: [k]) :: [*]
type instance ToTreeH '[] = '[]
type instance ToTreeH (a ': b ': bs) = (a :<|> b) ': ToTreeH bs

type family Len (a :: [k]) :: Nat
type instance Len '[] = 0
type instance Len (a ': bs) = 1 + Len bs

type family ToTree (a :: [k]) :: *
type instance ToTree (x ': y ': z ': xs) = ToTree (ToTreeH (x ': y ': z ': xs))
type instance ToTree (x ': y ': '[] ) = x :<|> y

type family Concat (a :: [Symbol]) :: *
type instance Concat (a ': b ': '[]) = a :<> b
type instance Concat (a ': b ': c ': cs) = a :<> b :<> Concat (c ': cs)

type family Intersperse (a :: k) (b :: [k]) :: [k]
type instance Intersperse a '[] = '[]
type instance Intersperse a (b ': '[]) = b ': '[]
type instance Intersperse a (b ': c ': cs) = b ': a ': Intersperse a (c ': cs)


getWord :: Int -> BitGet Word64
getWord i = do
	bs <- replicateM i getBool
	return $ foldl setBit 0 $ map fst $ filter snd $ zip [0..] bs



class MemRender a where
	render :: Proxy a -> BitGet String

instance KnownSymbol a => MemRender (a :: Symbol) where
	render = return . symbolVal

instance (MemRender a, MemRender b) => MemRender (a :<> b) where
	render _ = do
		sa <- render (Proxy :: Proxy a)
		sb <- render (Proxy :: Proxy b)
		return $ sa ++ sb

instance (MemRender a, MemRender b) => MemRender (a :<|> b) where
	render _ = do
		b <- getBool
		if b
			then render (Proxy :: Proxy a)
			else render (Proxy :: Proxy b)

instance KnownNat a => MemRender (a :: Nat) where
	render p = do
		w <- getWord (fromIntegral $ natVal p)
		return $ show w



main = undefined




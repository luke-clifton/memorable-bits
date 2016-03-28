{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Data.Memorable provides a bunch of type level operators for building
-- up a description of a pattern. The pattern can then be used to transform
-- arbitrary streams of bits into nice, human-readable and memorable strings.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Memorable.Theme.Fantasy
-- >>> renderMemorable rpgThings "\123\66\12\45\46\100"
-- "sacred axe of vampire splaterring"
--
-- Because this is a type level API, it is possible to write safe interfaces
-- to rendering specific things, and ensuring that the correct number of bits
-- can be represented.
--
-- Say you want to write a function which renders IPV4 addresses, but you
-- want the user of your function to choose the pattern. Simply specify that
-- the pattern requires an `ExactSize 32` in the constraints, and it is
-- guaranteed at compile time to only accept patterns of the correct length.
--
-- > renderIPV4
-- >     :: (ExactSize 32 a, MemRender a)
-- >     => Proxy a -> HostAddress -> String
-- > renderIPV4 = renderMemorableWord32
--
-- >>> :set -XDataKinds -XTypeOperators
-- >>> import Data.Memorable.Theme.Words
-- >>> import Network.Socket (inet_addr)
-- >>> type MyBadPattern = WordsBits 12 :<> "-" :<> WordsBits 12
-- >>> let myBadPatternProxy = Proxy :: Proxy MyBadPattern
-- >>> addr <- inet_addr "127.0.0.1"
-- >>> renderIPV4 myBadPatternProxy addr
-- <interactive>:32:1:
--     Couldn't match type ‘24’ with ‘32’
--     Expected type: 32
--       Actual type: MaxBits MyBadPattern
--     In the expression: renderIPV4 myBadPatternProxy addr
--     In an equation for ‘it’: it = renderIPV4 myBadPatternProxy addr
-- >>> type MyGoodPattern = MyBadPattern :<> "-" :<> WordsBits 8
-- >>> let myGoodPatternProxy = Proxy :: Proxy MyGoodPattern
-- >>> renderIPV4 myGoodPatternProxy addr
-- "could-that-rate"


module Data.Memorable
	( (:<>)
	, (:<->)
	, (:<.>)
	, (:<|>)
	, Choose
	, Concat
	, ConstantSize
	, Dec
	, Hex
	, ExactSize
	, Intersperse
	, MemRender (..)
	, getBit
	, getWord
	, MaxBits
	, MinBits
	, Prune
	, Number
	, NumberWithOffset
	, Proxy (..)
	, ToTree
	, getMaxBits
	, getMinBits
	, renderAll
	, renderMemorable
	, renderConstantSize
	, renderRandom
	) where

import Control.Monad.State
import Data.Binary.Bits.Get
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut, putWord8, putWord16be, putWord32be, putWord64be)
import Data.Bits
import Data.ByteString.Lazy (ByteString, pack)
import Data.List
import Data.Proxy
import Data.Word
import GHC.TypeLits
import GHC.Exts
import Numeric
import System.Random (randomIO)

---------------------------------------------------------------------
-- Basic type level combinators for describing patterns
---------------------------------------------------------------------


-- | Use this to select between two options.
-- This adds a single bit to the length of the longest branch.
data a :<|> b

-- | Concatenate two things together.
-- This adds no bits, but the resulting length is the sum of the two parts.
data a :<> b

type a :<-> b = a :<> "-" :<> b
type a :<.> b = a :<> "." :<> b

-- | Should be equivalent to `ToTree`, but more performant type checking.
data Choose (s :: [Symbol])

-- | Captures `n` bits and converts them to a string via the `nt` ("number type")
-- argument. See `Dec`, `Hex`.
type Number nt n = NumberWithOffset nt n 0

-- | Captures `n` bits and convertes them to a string via the `nt` ("number type")
-- argument after adding the offset. See `Dec`, `Hex`.
data NumberWithOffset nt (n :: Nat) (o :: Nat)

-- | Pad the `a` argument out to length `n` by taking the remaining bits and converting
-- them via `nt`. If padding is required, it is separated by `sep`.
data PadTo (sep :: Symbol) nt (n :: Nat) a

-- | Pad every branch to the length of the longest branch. See `PadTo`
type PadToMax s nt a = PadTo s nt (MaxBits a) a


---------------------------------------------------------------------
-- Utility type functions
---------------------------------------------------------------------

-- Helper for `ToTree`
type family ToTreeH (a :: [k]) :: [*] where
	ToTreeH '[] = '[]
	ToTreeH (a ': b ': bs) = (a :<|> b) ': ToTreeH bs

type family Len (a :: [k]) :: Nat where
	Len '[] = 0
	Len (a ': as) = 1 + Len as

-- | Convert a '[Symbol] to a balanced tree of `:<|>`. Each result has equal
-- probability of occurring. Length of the list must be a power of two.
type family ToTree (a :: [k]) :: * where
	ToTree (x ': y ': '[] ) = x :<|> y
	ToTree xs = ToTree (ToTreeH xs)

type family Concat (a :: [k]) :: * where
	Concat (a ': b ': '[]) = a :<> b
	Concat (a ': b ': cs) = a :<> b :<> Concat cs

type family Intersperse (a :: k) (b :: [k]) :: [k] where
	Intersperse a '[] = '[]
	Intersperse a (b ': '[]) = b ': '[]
	Intersperse a (b ': cs) = b ': a ': Intersperse a cs

type family If (p :: Bool) (a :: k) (b :: k) :: k where
	If True a b = a
	If False a b = b

-- | Reduce the size of a pattern by one bit.
type family Prune a :: *
type instance Prune (a :<> b) = Prune b
type instance Prune (a :<|> b) = a
type instance Prune (Number nt n) = Number nt (n - 1)

type PowerOfTwo n = (IsPowerOfTwo n ~ True)

type family IsPowerOfTwo (a :: Nat) :: Bool where
	IsPowerOfTwo 1 = True
	IsPowerOfTwo 2 = True
	IsPowerOfTwo 4 = True
	IsPowerOfTwo 8 = True
	IsPowerOfTwo 16 = True
	IsPowerOfTwo 32 = True
	IsPowerOfTwo 64 = True
	IsPowerOfTwo 128 = True
	IsPowerOfTwo 256 = True
	IsPowerOfTwo 512 = True
	IsPowerOfTwo 1024 = True
	IsPowerOfTwo 2048 = True
	IsPowerOfTwo 4096 = True
	IsPowerOfTwo 4096 = True
	IsPowerOfTwo 8192 = True

type family BitsInPowerOfTwo (a :: Nat) :: Nat where
	BitsInPowerOfTwo 1 = 0
	BitsInPowerOfTwo 2 = 1
	BitsInPowerOfTwo 4 = 2
	BitsInPowerOfTwo 8 = 3
	BitsInPowerOfTwo 16 = 4
	BitsInPowerOfTwo 32 = 5
	BitsInPowerOfTwo 64 = 6
	BitsInPowerOfTwo 128 = 7
	BitsInPowerOfTwo 256 = 8
	BitsInPowerOfTwo 512 = 9
	BitsInPowerOfTwo 1024 = 10
	BitsInPowerOfTwo 2048 = 11
	BitsInPowerOfTwo 4096 = 12
	BitsInPowerOfTwo 8192 = 13

-- | Determines the maximum number of bits that a pattern will consume.
type family MaxBits (a :: k) :: Nat
type instance MaxBits (a :: Symbol) = 0
type instance MaxBits (a :<> b) = MaxBits a + MaxBits b
type instance MaxBits (a :<|> b) = 1 + If (MaxBits a <=? MaxBits b) (MaxBits b) (MaxBits a)
type instance MaxBits (NumberWithOffset nt a o) = a
type instance MaxBits (PadTo s nt n a) = n
type instance MaxBits (Choose ss) = (BitsInPowerOfTwo (Len ss))

-- | Determines the minimum number of bits that a pattern will consume.
type family MinBits (a :: k) :: Nat
type instance MinBits (a :: Symbol) = 0
type instance MinBits (a :<> b) = MinBits a + MinBits b
type instance MinBits (a :<|> b) = 1 + If (MinBits a <=? MinBits b) (MinBits a) (MinBits b)
type instance MinBits (NumberWithOffset nt a o) = a
type instance MinBits (PadTo s nt n a) = n
type instance MinBits (Choose ss) = (BitsInPowerOfTwo (Len ss))

-- | Some tasks require that all branches through a pattern consume the same number of bits.
type ConstantSize a = (MinBits a ~ MaxBits a)

-- | Some tasks require an exact number of bits to be consumed.
type ExactSize (n :: Nat) a = (ConstantSize a, MaxBits a ~ n)

-- | Value level wrapper around `MaxBits`
getMaxBits :: forall a. (KnownNat (MaxBits a)) => Proxy a -> Integer
getMaxBits _ = natVal (Proxy :: Proxy (MaxBits a))

-- | Value level wrapper around `MinBits`
getMinBits :: forall a. (KnownNat (MinBits a)) => Proxy a -> Integer
getMinBits _ = natVal (Proxy :: Proxy (MinBits a))

---------------------------------------------------------------------
-- BitPull
---------------------------------------------------------------------

-- | Context in which we run our renderers. Allows one to pull of
-- a single bit at a time, or to query for many bits and return them
-- as an `Integer`. It also keeps track of how many bits we have consumed.
newtype BitPull r = BitPull { unBitPull :: StateT Int BitGet r }
	deriving (Functor, Applicative, Monad)

-- | Get a single bit from the stream as a Bool.
getBit :: BitPull Bool
getBit = BitPull $ do
	modify succ
	lift $ getBool

-- | Convert the next `n` bits into an unsigned integer.
getWord :: Int -> BitPull Integer
getWord n = do
	bs <- replicateM n getBit
	return $ foldl' setBit 0 $ map fst $ filter snd $ zip [0..] bs

-- | Returns the number of bits consumed so far.
getConsumedCount :: BitPull Int
getConsumedCount = BitPull get


---------------------------------------------------------------------
-- MemRender
---------------------------------------------------------------------

-- | The class that implements the main rendering function.
class MemRender a where
	render :: Proxy a -> BitPull String

instance KnownSymbol a => MemRender (a :: Symbol) where
	render = return . symbolVal

instance (MemRender a, MemRender b) => MemRender (a :<> b) where
	render _ = do
		sa <- render (Proxy :: Proxy a)
		sb <- render (Proxy :: Proxy b)
		return $ sa ++ sb

instance (MemRender a, MemRender b) => MemRender (a :<|> b) where
	render _ = do
		b <- getBit
		if b
			then render (Proxy :: Proxy a)
			else render (Proxy :: Proxy b)


instance (NumberRender nt, KnownNat a, KnownNat o) => MemRender (NumberWithOffset nt a o) where
	render _ = do
		let
			o = natVal (Proxy :: Proxy o)
			b = natVal (Proxy :: Proxy a)
		w <- getWord $ fromIntegral b
		return $ renderNumber (Proxy :: Proxy nt) b (w + o)


instance (MemRender a, MaxBits a <= n, NumberRender nt, KnownNat n, KnownSymbol s) => MemRender (PadTo s nt n a) where
	render _ = do
		s1 <- render (Proxy :: Proxy a)
		c <- getConsumedCount
		let
			n = natVal (Proxy :: Proxy n)
			ntp = Proxy :: Proxy nt
			diff = n - fromIntegral c
			sep = symbolVal (Proxy :: Proxy s)
		case diff of
			0 -> return s1
			_ -> do
				d <- getWord $ fromIntegral diff
				return $ s1 ++ sep ++ renderNumber ntp diff d


---------------------------------------------------------------------
-- NumberRender
---------------------------------------------------------------------

-- | Class for capturing how to render numbers.
class NumberRender n where
	renderNumber :: Proxy n -> Integer -> Integer -> String


-- | Render numbers as decimal numbers
data Dec

instance NumberRender Dec where
	renderNumber _ _ = show


-- | Render numbers as hexadecimal numbers
data Hex

instance NumberRender Hex where
	renderNumber _ _ i = showHex i ""


---------------------------------------------------------------------
-- KnownSymbolList
---------------------------------------------------------------------

class KnownSymbolList a where
	symbolListVal :: Proxy a -> [String]

instance KnownSymbolList '[] where
	symbolListVal _ = []

instance (KnownSymbol s, KnownSymbolList ss) => KnownSymbolList (s ': ss) where
	symbolListVal _ = symbolVal (Proxy :: Proxy s) : symbolListVal (Proxy :: Proxy ss)
	

instance
	( KnownSymbolList ss
	, PowerOfTwo (Len ss)
	, KnownNat (Len ss)
	, KnownNat (BitsInPowerOfTwo (Len ss))
	) => MemRender (Choose ss) where
	render _ = do
		let
			l = natVal (Proxy :: Proxy (Len ss))
			ss = symbolListVal (Proxy :: Proxy ss)
			bs = natVal (Proxy :: Proxy (BitsInPowerOfTwo (Len ss)))
		i <- getWord $ fromIntegral bs
		return $ ss !! fromIntegral i


---------------------------------------------------------------------
-- Rendering functions for users
---------------------------------------------------------------------

-- | Render a `ByteString` as a more memorable `String`.
renderMemorable
	:: MemRender a
	=> Proxy a -> ByteString -> String
renderMemorable p =
	runGet (runBitGet . flip evalStateT 0 . unBitPull $ render p)

renderMemorableWord8
	:: (MemRender a, ExactSize 8 a)
	=> Proxy a -> Word8 -> String
renderMemorableWord8 p w = renderMemorable p (runPut $ putWord8 w)

renderMemorableWord16
	:: (MemRender a, ExactSize 16 a)
	=> Proxy a -> Word16 -> String
renderMemorableWord16 p w = renderMemorable p (runPut $ putWord16be w)

renderMemorableWord32
	:: (MemRender a, ExactSize 32 a)
	=> Proxy a -> Word32 -> String
renderMemorableWord32 p w = renderMemorable p (runPut $ putWord32be w)

renderMemorableWord64
	:: (MemRender a, ExactSize 64 a)
	=> Proxy a -> Word64 -> String
renderMemorableWord64 p w = renderMemorable p (runPut $ putWord64be w)

-- | Render a constant size pattern.
renderConstantSize
	:: (MemRender a, ConstantSize a)
	=> Proxy a -> ByteString -> String
renderConstantSize = renderMemorable

-- | Generate a random string.
renderRandom
	:: forall a. (MemRender a, KnownNat (MaxBits a))
	=> Proxy a -> IO String
renderRandom p = do
	let
		nBits = getMaxBits p
		nBytes = fromIntegral $ nBits `div` 8 + 1
	bs <- pack <$> replicateM nBytes randomIO
	return $ renderMemorable p bs

flipByte :: Word8 -> Word8
flipByte b =
	fromIntegral $
		( (fromIntegral b * (0x0202020202 :: Word64))
		.&. 0x010884422010
		) `mod` 1023

-- | Render all strings that a pattern will produce.
renderAll
	:: forall a. (MemRender a, KnownNat (MaxBits a))
	=> Proxy a -> [String]
renderAll p = map (renderMemorable p) allByteStrings
	where
		nBits = getMaxBits p
		nBytes = fromIntegral $ nBits `div` 8 + 1
		allByteStrings =
			take (2 ^ nBits)
				$ map (pack . reverse)
				$ replicateM nBytes 
				$ map flipByte [minBound .. maxBound]

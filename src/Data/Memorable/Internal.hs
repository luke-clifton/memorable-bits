{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Memorable.Internal where

import Control.Arrow (first)
import Control.Applicative
import Data.Maybe
import Control.Monad.State
import Control.Monad.Writer
import Data.Binary.Bits.Get
import qualified Data.Binary.Bits.Put as BP
import Data.Binary.Get (runGet)
import qualified Data.Binary
import Data.Binary.Put (runPut, putWord8, putWord16be, putWord32be, putWord64be)
import Data.Bits
import Data.Type.Equality
import Data.Type.Bool
import Data.ByteString.Lazy (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Proxy
import Data.Word
import Data.Int
import GHC.TypeLits
import GHC.Exts
import Numeric
import System.Random (randomIO)

-- | Choice between two sub patterns. It's not safe to use this directly.
-- Use `.|` instead.
data a :| b

-- | Append two patterns together by doing the first, then the second. See
-- also `.-`
data a :- b

-- | Proxy version of `:|`. It also constraints the two subpatterns to
-- being the same depth.
(.|) :: (Depth a ~ Depth b) => Proxy a -> Proxy b -> Proxy (a :| b)
_ .| _ = Proxy

-- | Proxy version of `:-`.
-- This adds no bits, but the resulting length is the sum of the two parts.
(.-) :: Proxy a -> Proxy b -> Proxy (a :- b)
_ .- _ = Proxy

-- | Captures `n` bits and converts them to a string via the `nt` ("number type")
-- argument. See `Dec`, `Hex`.
type Number nt n = NumberWithOffset nt n 0

-- | Captures `n` bits and convertes them to a string via the `nt` ("number type")
-- argument after adding the offset. See `Dec`, `Hex`.
data NumberWithOffset nt (n :: Nat) (o :: Nat)

-- | Pad the `a` argument out to length `n` by taking the remaining bits
-- and converting them via `nt` (see `Dec` and `Hex`). If padding is required,
-- it is separated by a dash.
data PadTo nt (n :: Nat) a


---------------------------------------------------------------------
-- Utility type functions
---------------------------------------------------------------------

-- Helper for `ToTree`
type family ToTreeH (a :: [k]) :: [*] where
    ToTreeH '[] = '[]
    ToTreeH (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': x9 ': x10 ': x11 ': x12 ': x13 ': x14 ': x15 ': x16 ': x17 ': x18 ': x19 ': x20 ': x21 ': x22 ': x23 ': x24 ': x25 ': x26 ': x27 ': x28 ': x29 ': x30 ': x31 ': x32 ': x33 ': x34 ': x35 ': x36 ': x37 ': x38 ': x39 ': x40 ': x41 ': x42 ': x43 ': x44 ': x45 ': x46 ': x47 ': x48 ': x49 ': x50 ': x51 ': x52 ': x53 ': x54 ': x55 ': x56 ': x57 ': x58 ': x59 ': x60 ': x61 ': x62 ': x63 ': x64 ': xs) = ToTree64 (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': x9 ': x10 ': x11 ': x12 ': x13 ': x14 ': x15 ': x16 ': x17 ': x18 ': x19 ': x20 ': x21 ': x22 ': x23 ': x24 ': x25 ': x26 ': x27 ': x28 ': x29 ': x30 ': x31 ': x32 ': x33 ': x34 ': x35 ': x36 ': x37 ': x38 ': x39 ': x40 ': x41 ': x42 ': x43 ': x44 ': x45 ': x46 ': x47 ': x48 ': x49 ': x50 ': x51 ': x52 ': x53 ': x54 ': x55 ': x56 ': x57 ': x58 ': x59 ': x60 ': x61 ': x62 ': x63 ': x64 ': xs)
    ToTreeH as = ToTree2 as

type family ToTree2 (as :: [k]) :: [*] where    
    ToTree2 '[] = '[]
    ToTree2 (a ': b ': bs) = (a :| b) ': ToTree2 bs

type family ToTree64 (as :: [k]) :: [*] where    
    ToTree64 '[] = '[]
    ToTree64 (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': x9 ': x10 ': x11 ': x12 ': x13 ': x14 ': x15 ': x16 ': x17 ': x18 ': x19 ': x20 ': x21 ': x22 ': x23 ': x24 ': x25 ': x26 ': x27 ': x28 ': x29 ': x30 ': x31 ': x32 ': x33 ': x34 ': x35 ': x36 ': x37 ': x38 ': x39 ': x40 ': x41 ': x42 ': x43 ': x44 ': x45 ': x46 ': x47 ': x48 ': x49 ': x50 ': x51 ': x52 ': x53 ': x54 ': x55 ': x56 ': x57 ': x58 ': x59 ': x60 ': x61 ': x62 ': x63 ': x64 ': xs) =
        (
            (
                (
                    (
                        (
                            (x1 :| x2) :| (x3 :| x4)
                        ) :|
                        (
                            (x5 :| x6) :| (x7 :| x8)
                        )
                    ) :|
                    (
                        (
                            (x9 :| x10) :| (x11 :| x12)
                        ) :|
                        (
                            (x13 :| x14) :| (x15 :| x16)
                        )
                    )
                ) :|
                (
                    (
                        (
                            (x17 :| x18) :| (x19 :| x20)
                        ) :|
                        (
                            (x21 :| x22) :| (x23 :| x24)
                        )
                    ) :|
                    (
                        (
                            (x25 :| x26) :| (x27 :| x28)
                        ) :|
                        (
                            (x29 :| x30) :| (x31 :| x32)
                        )
                    )
                )
            )  :|
            (
                (
                    (
                        (
                            (x33 :| x34) :| (x35 :| x36)
                        ) :|
                        (
                            (x37 :| x38) :| (x39 :| x40)
                        )
                    ) :|
                    (
                        (
                            (x41 :| x42) :| (x43 :| x44)
                        ) :|
                        (
                            (x45 :| x46) :| (x47 :| x48)
                        )
                    )
                ) :|
                (
                    (
                        (
                            (x49 :| x50) :| (x51 :| x52)
                        ) :|
                        (
                            (x53 :| x54) :| (x55 :| x56)
                        )
                    ) :|
                    (
                        (
                            (x57 :| x58) :| (x59 :| x60)
                        ) :|
                        (
                            (x61 :| x62) :| (x63 :| x64)
                        )
                    )
                )
            ) 
        ) ': ToTree64 xs

type family Len (a :: [Symbol]) :: Nat where
    Len (a ': b ': c ': d ': e ': f ': g ': h ': i ': j ': k ': l ': m ': n ': o ': p ': q ': r ': s ': t ': u ': v ': w ': x ': y ': z ': as) = Len as + 26
    Len (a ': as) = Len as + 1
    Len '[] = 0

-- | Convert a @'[Symbol]@ to a balanced tree of `:|`. Each result has equal
-- probability of occurring. Length of the list must be a power of two.
type family ToTree (a :: [k]) :: * where
    ToTree (x ': y ': '[] ) = x :| y
    ToTree '[(x :| y)] = x :| y
    ToTree xs = ToTree (ToTreeH xs)

type family Concat (a :: [k]) :: * where
    Concat (a ': b ': '[]) = a :- b
    Concat (a ': b ': cs) = a :- b :- Concat cs

type family Intersperse (a :: k) (b :: [k]) :: [k] where
    Intersperse a '[] = '[]
    Intersperse a (b ': '[]) = b ': '[]
    Intersperse a (b ': cs) = b ': a ': Intersperse a cs


type family LeftSide (a :: *) :: * where
    LeftSide (a :| b) = a

-- | Shrink a branching pattern by discarding the right hand side.
leftSide :: Proxy (a :| b) -> Proxy a
leftSide _ = Proxy

-- | Shrink a branching pattern by discarding the left hand side.
rightSide :: Proxy (a :| b) -> Proxy b
rightSide _ = Proxy


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

-- | This type family is used to make sure that the total number of bit's
-- covered by a pattern is a multiple of 8.
type family MultipleOf8 a :: Constraint where
    MultipleOf8 0 = ()
    MultipleOf8 1 = TypeError (Text "Not a multiple of 8")
    MultipleOf8 2 = TypeError (Text "Not a multiple of 8") 
    MultipleOf8 3 = TypeError (Text "Not a multiple of 8") 
    MultipleOf8 4 = TypeError (Text "Not a multiple of 8") 
    MultipleOf8 5 = TypeError (Text "Not a multiple of 8") 
    MultipleOf8 6 = TypeError (Text "Not a multiple of 8") 
    MultipleOf8 7 = TypeError (Text "Not a multiple of 8") 
    MultipleOf8 x = MultipleOf8 (x - 8)

type family Find a as :: Bool where
    Find a '[] = 'False
    Find a (a ': as) = 'True
    Find a (b ': a ': as) = 'True
    Find a (c ': b ': a ': as) = 'True
    Find a (d ': c ': b ': a ': as) = 'True
    Find a (e ': d ': c ': b ': a ': as) = 'True
    Find a (f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (n ': m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (o ': n ': m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (p ': o ': n ': m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (q ': p ': o ': n ': m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (r ': q ': p ': o ': n ': m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (s ': r ': q ': p ': o ': n ': m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (t ': s ': r ': q ': p ': o ': n ': m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (u ': t ': s ': r ': q ': p ': o ': n ': m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (v ': u ': t ': s ': r ': q ': p ': o ': n ': m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (w ': v ': u ': t ': s ': r ': q ': p ': o ': n ': m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (x ': w ': v ': u ': t ': s ': r ': q ': p ': o ': n ': m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (y ': x ': w ': v ': u ': t ': s ': r ': q ': p ': o ': n ': m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (z ': y ': x ': w ': v ': u ': t ': s ': r ': q ': p ': o ': n ': m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': a ': as) = 'True
    Find a (z ': y ': x ': w ': v ': u ': t ': s ': r ': q ': p ': o ': n ': m ': l ': k ': j ': i ': h ': g ': f ': e ': d ': c ': b ': aa ': as) = Find a as
    Find a (b ': as ) = Find a as


type family HasDups (a :: [Symbol]) :: Bool where
    HasDups (a ': as) = Find a as || HasDups as
    HasDups '[] = 'False

type family NoDups (a :: [Symbol]) :: Constraint where
    NoDups (a ': as) = If (Find a as) (TypeError (Text "Pattern is ambiguous because of " :<>: ShowType a)) (NoDups as)
    NoDups '[] = ()

-- | Determines the number of bits that a pattern will consume.
type family Depth (a :: k) :: Nat where
    Depth (a :: Symbol) = 0
    Depth (a :- b) = Depth a + Depth b
    Depth (a :| b) = 1 + Depth a
    Depth (NumberWithOffset nt a o) = a
    Depth (PadTo nt n a) = n

-- | Get the depth of a pattern as a value-level `Integer`
getDepth :: forall a. KnownNat (Depth a) => Proxy a -> Integer
getDepth _ = natVal (Proxy :: Proxy (Depth a))


---------------------------------------------------------------------
-- Utility functions
---------------------------------------------------------------------

type family NTimes (n :: Nat) (p :: *) where
    NTimes 1 a = a
    NTimes n a = a :- NTimes (n - 1) a

-- | Put five things next to each other.
five :: Proxy a -> Proxy (a :- a :- a :- a :- a)
five _ = Proxy

-- | Put four things next to each other.
four :: Proxy a -> Proxy (a :- a :- a :- a)
four _ = Proxy

-- | Put three things next to each other.
three :: Proxy a -> Proxy (a :- a :- a)
three _ = Proxy

-- | Put two things next to each other.
two :: Proxy a -> Proxy (a :- a)
two _ = Proxy

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
    lift getBool

-- | Convert the next `n` bits into an unsigned integer.
getWord :: Int -> BitPull Integer
getWord n = do
    bs <- replicateM n getBit
    return $ foldl' setBit 0 $ map fst $ filter snd $ zip [0..] bs

-- | Returns the number of bits consumed so far.
getConsumedCount :: BitPull Int
getConsumedCount = BitPull get

newtype BitPush r = BitPush { unBitPush :: StateT ([Bool],String) Maybe r }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

putBit :: Bool -> BitPush ()
putBit b = BitPush $ modify (first (b:))

putWord :: Int -> Integer -> BitPush ()
putWord b w = mapM_ (putBit . testBit w) [0 ..  b - 1 ]

getWrittenCount :: BitPush Int
getWrittenCount = BitPush (length . fst <$> get)

consume :: (Char -> Bool) -> BitPush String
consume p = BitPush $ do
    (bs, ss) <- get
    let
        s = takeWhile p ss
        s' = dropWhile p ss
    put (bs, s')
    return s

---------------------------------------------------------------------
-- MemRender
---------------------------------------------------------------------

-- | The class that implements the main rendering function.
class MemRender a where
    render :: Proxy a -> BitPull String

symbolString :: KnownSymbol a => Proxy a -> String
symbolString = concatMap tr . symbolVal
    where
        tr '-' = "\\-"
        tr '\\' = "\\\\"
        tr c = [c]


instance (KnownSymbol a) => MemRender (a :: Symbol) where
    render = return . symbolString

instance (MemRender a, MemRender b) => MemRender (a :- b) where
    render _ = do
        sa <- render (Proxy :: Proxy a)
        sb <- render (Proxy :: Proxy b)
        return $ sa ++ "-" ++ sb

instance (MemRender a, MemRender b) => MemRender (a :| b) where
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

instance (MemRender a, Depth a <= n, NumberRender nt, KnownNat n) => MemRender (PadTo nt n a) where
    render _ = do
        s1 <- render (Proxy :: Proxy a)
        c <- getConsumedCount
        let
            n = natVal (Proxy :: Proxy n)
            ntp = Proxy :: Proxy nt
            diff = n - fromIntegral c
        case diff of
            0 -> return s1
            _ -> do
                d <- getWord $ fromIntegral diff
                return $ s1 ++ "-" ++ renderNumber ntp diff d

---------------------------------------------------------------------
-- NumberRender
---------------------------------------------------------------------

-- | Class for capturing how to render numbers.
class NumberRender n where
    renderNumber :: Proxy n -> Integer -> Integer -> String
    readNumber :: Proxy n -> Integer -> String -> Maybe Integer


-- | Render numbers as decimal numbers
data Dec

instance NumberRender Dec where
    renderNumber _ _ = show
    readNumber _ _ input = case readDec input of
        [(v,"")] -> Just v
        _ -> Nothing


-- | Render numbers as hexadecimal numbers
data Hex

instance NumberRender Hex where
    renderNumber _ _ i = showHex i ""
    readNumber _ _ input = case readHex input of
        [(v,"")] -> Just v
        _ -> Nothing

---------------------------------------------------------------------
-- Rendering functions for users
---------------------------------------------------------------------

newtype M (n :: Nat) = M Data.Binary.Put

-- | A monoidal-esque combinator for appending @`M` n@ together.
-- Useful for product instances of @`Memorable`@.
smoosh :: M a -> M b -> M (a + b)
smoosh (M x) (M y) = M (x <> y)

-- | Class for all things that can be converted to memorable strings.
-- See `renderMemorable` for how to use.
class (MultipleOf8 (MemLen a)) => Memorable a where
    -- Do not lie. Use @`testMemLen`@.
    type MemLen a :: Nat
    renderMem :: a -> M (MemLen a)

-- | Use this with tasty-quickcheck (or your prefered testing framework) to
-- make sure you aren't lying about `MemLen`.
-- 
-- @
-- testProperty "MemLen Word8" $ forAll (arbitrary :: Gen Word8) `testMemLen`
-- @
testMemLen :: forall a. (KnownNat (MemLen a), Memorable a) => a -> Bool
testMemLen a =
    let
        M p = renderMem a
        bs = runPut p
        l = natVal (Proxy :: Proxy (MemLen a))
        bl = 8 * fromIntegral (BL.length bs)
    in
        l == bl

instance Memorable Word8 where
    type MemLen Word8 = 8
    renderMem = M . Data.Binary.put

instance Memorable Word16 where
    type MemLen Word16 = 16
    renderMem = M . Data.Binary.put

instance Memorable Word32 where
    type MemLen Word32 = 32
    renderMem = M . Data.Binary.put

instance Memorable Word64 where
    type MemLen Word64 = 64
    renderMem = M . Data.Binary.put

instance Memorable Int8 where
    type MemLen Int8 = 8
    renderMem = M . Data.Binary.put

instance Memorable Int16 where
    type MemLen Int16 = 16
    renderMem = M . Data.Binary.put

instance Memorable Int32 where
    type MemLen Int32 = 32
    renderMem = M . Data.Binary.put

instance Memorable Int64 where
    type MemLen Int64 = 64
    renderMem = M . Data.Binary.put

instance (Memorable a, Memorable b, MultipleOf8 (MemLen (a,b))) => Memorable (a,b) where
    type MemLen (a,b) = MemLen a + MemLen b
    renderMem (a,b) = smoosh (renderMem a) (renderMem b)

instance (Memorable a, Memorable b, Memorable c, MultipleOf8 (MemLen (a,b,c))) => Memorable (a,b,c) where
    type MemLen (a,b,c) = MemLen a + MemLen b + MemLen c
    renderMem (a,b,c) = renderMem a `smoosh` renderMem b `smoosh` renderMem c

instance (Memorable a, Memorable b, Memorable c, Memorable d, MultipleOf8 (MemLen (a,b,c,d))) => Memorable (a,b,c,d) where
    type MemLen (a,b,c,d) = MemLen a + MemLen b + MemLen c + MemLen d
    renderMem (a,b,c,d) = renderMem a `smoosh` renderMem b `smoosh` renderMem c `smoosh` renderMem d

instance (Memorable a, Memorable b, Memorable c, Memorable d, Memorable e, MultipleOf8 (MemLen (a,b,c,d,e))) => Memorable (a,b,c,d,e) where
    type MemLen (a,b,c,d,e) = MemLen a + MemLen b + MemLen c + MemLen d + MemLen e
    renderMem (a,b,c,d,e) = renderMem a `smoosh` renderMem b `smoosh` renderMem c `smoosh` renderMem d `smoosh` renderMem e

instance (Memorable a, Memorable b, Memorable c, Memorable d, Memorable e, Memorable f, MultipleOf8 (MemLen (a,b,c,d,e,f))) => Memorable (a,b,c,d,e,f) where
    type MemLen (a,b,c,d,e,f) = MemLen a + MemLen b + MemLen c + MemLen d + MemLen e + MemLen f
    renderMem (a,b,c,d,e,f) = renderMem a `smoosh` renderMem b `smoosh` renderMem c `smoosh` renderMem d `smoosh` renderMem e `smoosh` renderMem f

-- | Take an pattern and a @`M` n@ and produces your human readable string.
-- See `renderMemorable`.
renderM :: (Depth a ~ n, MemRender a) => Proxy a -> M n -> String
renderM p (M b) = renderMemorableByteString p (runPut b)

-- | This is the function to use when you want to turn your values into a
-- memorable strings.
renderMemorable :: (MemRender p, Depth p ~ MemLen a, Memorable a) => Proxy p -> a -> String
renderMemorable p a = renderM p (renderMem a)

-- | Render a `ByteString` as a more memorable `String`.
renderMemorableByteString
    :: MemRender a
    => Proxy a -> ByteString -> String
renderMemorableByteString p =
    runGet (runBitGet . flip evalStateT 0 . unBitPull $ render p)

-- | Generate a random string.
renderRandom
    :: forall a. (MemRender a, KnownNat (Depth a))
    => Proxy a -> IO String
renderRandom p = do
    let
        nBits = getDepth p
        nBytes = fromIntegral $ nBits `div` 8 + 1
    bs <- pack <$> replicateM nBytes randomIO
    return $ renderMemorableByteString p bs

flipByte :: Word8 -> Word8
flipByte b =
    fromIntegral $
        ( (fromIntegral b * (0x0202020202 :: Word64))
        .&. 0x010884422010
        ) `mod` 1023

-- | Render all strings that a pattern will produce.
renderAll
    :: forall a. (MemRender a, KnownNat (Depth a))
    => Proxy a -> [String]
renderAll p = map (renderMemorableByteString p) allByteStrings
    where
        nBits = getDepth p
        nBytes = fromIntegral $ nBits `div` 8 + 1
        allByteStrings =
            take (2 ^ nBits)
                $ map (pack . reverse)
                $ replicateM nBytes 
                $ map flipByte [minBound .. maxBound]


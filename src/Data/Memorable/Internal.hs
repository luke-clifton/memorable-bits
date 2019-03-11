{-# LANGUAGE CPP #-}
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
module Data.Memorable.Internal where

import Control.Arrow (first)
import Text.Printf
import Control.Applicative
import Control.Monad.Except
import Data.Maybe
import Data.List.Split
import Control.Monad.State
import Control.Monad.Writer
import Data.Hashable
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.Binary
import Data.Bits
import Data.Bits.Coding hiding (putUnaligned)
import Data.Bytes.Put
import Data.Bytes.Get
import Data.Type.Equality
import Data.Type.Bool
import Data.ByteString.Lazy (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.List
import Data.Proxy
import Data.Word
import Data.Int
import GHC.TypeLits
import GHC.Exts
import Numeric
import System.Random (randomIO)
#ifdef DATA_DWORD
import Data.DoubleWord
#endif
#ifdef NETWORK_IP
import Network.IP.Addr
#endif
#ifdef CRYPTONITE
import Data.ByteArray (convert)
import Crypto.Hash hiding (hash)
#endif
#ifdef HASHABLE
import Data.Hashable
#endif

-- | Choice between two sub patterns. It's not safe to use this directly.
-- Use `.|` instead.
--
-- Also, if you are parsing back rendered phrases, you must make sure that
-- the selected word is enough to choose a side. That is, `a` and `b` must
-- have unique first words. This is NOT checked, as it causes a HUGE
-- compile-time performance hit. If we can make it performant it may be
-- checked one day.
data a :| b

-- | Append two patterns together by doing the first, then the second. See
-- also `.-`
data a :- b

-- | Proxy version of `:|`. It also constraints the two subpatterns to
-- being the same depth. Use this to add an extra bit to the pattern depth,
-- where the bit chooses to proceed down either the left or right side.
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> import Data.Word
-- >>> let myPattern = padHex (Proxy @"foo" .| Proxy @"bar")
-- >>> renderMemorable myPattern (0x00 :: Word8)
-- "bar-00"
-- >>> renderMemorable myPattern (0xff :: Word8)
-- "foo-7f"
--
-- See also 'ToTree'
--
-- WARNING: Each side of the split must be unique. See the warning about `:|`.
(.|) :: (Depth a ~ Depth b) => Proxy a -> Proxy b -> Proxy (a :| b)
_ .| _ = Proxy

-- | Proxy version of `:-`.
-- The new pattern depth is the sum of the two parts.
-- >>> import Data.Word
-- >>> import Data.Memorable.Theme.Words
-- >>> let myPattern = words8 .- words8
-- >>> renderMemorable myPattern (0xabcd :: Word16)
-- "ages-old"
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
--
-- See `padHex` and `padDec` for convinence functions.
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
-- probability of occurring. Length of the list must be a power of two. This
-- is very useful for converting long lists of words into a usable pattern.
--
-- >>> :kind! ToTree '["a", "b", "c", "d"]
-- ToTree '["a", "b", "c", "d"] :: *
-- = ("a" :| "b") :| ("c" :| "d")
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


-- | Useful to prevent haddock from expanding the type.
type family LeftSide (a :: *) :: * where
    LeftSide (a :| b) = a

-- | Useful to prevent haddock from expanding the type.
type family RightSide (a :: *) :: * where
    RightSide (a :| b) = b

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

-- | Get the depth of a pattern as a value-level `Integer`.
-- >>> :set -XTypeApplications -XDataKinds
-- >>> getDepth (Proxy @"foo" .| Proxy @"bar")
-- 1
getDepth :: forall a. KnownNat (Depth a) => Proxy a -> Integer
getDepth _ = natVal (Proxy :: Proxy (Depth a))


---------------------------------------------------------------------
-- Utility functions
---------------------------------------------------------------------

type family NTimes (n :: Nat) (p :: *) where
    NTimes 1 a = a
    NTimes n a = a :- NTimes (n - 1) a

-- | Put five things next to each other.
-- Same as using '.-' repeatedly
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

-- | Pad this pattern out with hex digits. Useful when you want some human
-- readability, but also want full coverage of the data. See 'Hex' for details.
--
-- >>> import Data.Word
-- >>> import Data.Memorable.Theme.Fantasy
-- >>> renderMemorable (padHex rpgWeapons) (0xdeadbeef01020304 :: Word64)
-- "sacred-club-of-ghoul-charming-eef01020304"
padHex :: forall n a. Proxy a -> Proxy (PadTo Hex n a)
padHex _ = Proxy

-- | Pad with decimal digits. See 'padHex' and 'Dec' for details. This does
-- not pad with 0's
padDec :: forall n a. Proxy a -> Proxy (PadTo Dec n a)
padDec _ = Proxy

-- | A single hex number consuming 4 bits (with leading 0's).
hex4 :: Proxy (Number Hex 4)
hex4 = Proxy

-- | A single hex number consuming 8 bits (with leading 0's).
hex8 :: Proxy (Number Hex 8)
hex8 = Proxy

-- | A single hex number consuming 16 bits (with leading 0's).
hex16 :: Proxy (Number Hex 16)
hex16 = Proxy

-- | A single hex number consuming 32 bits (with leading 0's).
hex32 :: Proxy (Number Hex 32)
hex32 = Proxy

-- | A single hex number consuming `n` bits, which it will try and figure
-- out from context (with leading 0's).
hex :: Proxy (Number Hex n)
hex = Proxy

-- | A single decimal number consuming 4 bits (no leading 0's)
dec4 :: Proxy (Number Dec 4)
dec4 = Proxy

-- | A single decimal number consuming 8 bits (no leading 0's)
dec8 :: Proxy (Number Dec 8)
dec8 = Proxy

-- | A single decimal number consuming 16 bits (no leading 0's)
dec16 :: Proxy (Number Dec 16)
dec16 = Proxy

-- | A single decimal number consuming 32 bits (no leading 0's)
dec32 :: Proxy (Number Dec 32)
dec32 = Proxy

-- | A single decimal number consuming `n` bits, which it will try and figure
-- out from context (no leading 0's)
dec :: Proxy (Number Dec n)
dec = Proxy

---------------------------------------------------------------------
-- MemRender
---------------------------------------------------------------------

-- | The class that implements the main rendering function.
class MemRender a where
    render :: Proxy a -> Coding Get String
    parser :: Proxy a -> ExceptT String (State ([String], Coding PutM ())) ()

addBits :: Coding PutM () -> ExceptT String (State ([String], Coding PutM ())) ()
addBits c = do
    (s,cs) <- get
    put (s,cs >> c)

symbolString :: KnownSymbol a => Proxy a -> String
symbolString = concatMap tr . symbolVal
    where
        tr '-' = "\\_"
        tr '\\' = "\\\\"
        tr c = [c]

stringSymbol :: String -> String
stringSymbol [] = []
stringSymbol ('\\':'\\':rest) = '\\' : stringSymbol rest
stringSymbol ('\\':'_':rest) = '-' : stringSymbol rest
stringSymbol (a:rest) = a : stringSymbol rest

parsePhrase :: MemRender p => Proxy p -> String -> Maybe ByteString
parsePhrase p input =
    let
        tokens = map stringSymbol $ splitOn "-" input
        stm = runExceptT (parser p)
        (e,(_,cdm)) = runState stm (tokens, pure ())
        ptm = runCoding (cdm <* Data.Bytes.Put.flush) (\a _ _ -> pure a) 0 0
    in
        case e of
            Left _ -> Nothing
            Right () -> Just $ runPutL ptm

-- | Turn a memorable string back into a 'Memorable' value.
parseMemorable :: (Memorable a, MemRender p, MemLen a ~ Depth p) => Proxy p -> String -> Maybe a
parseMemorable p input =
    let
        bs = parsePhrase p input
    in runParser <$> bs

-- | Convert a memorable string into a different memorable string.
--
-- Useful for things like taking an existing md5, and converting it
-- into a memorable one.
--
-- >>> :set -XTypeApplications -XDataKinds
-- >>> import Data.Memorable.Theme.Words
-- >>> rerender hex (padHex @128 $ four words10) "2d4fbe4d5db8748c931b85c551d03360"
-- Just "lurk-lash-atop-hole-b8748c931b85c551d03360"
rerender :: (MemRender a, MemRender b, Depth a ~ Depth b) => Proxy a -> Proxy b -> String -> Maybe String
rerender a b input = renderMemorableByteString b <$> parsePhrase a input

instance (KnownSymbol a) => MemRender (a :: Symbol) where
    render = return . symbolString
    parser p = do
        (ss,cs) <- get
        case ss of
            [] -> empty
            s:ss' ->
                if s == symbolVal p
                    then put (ss',cs)
                    else empty
        

instance (MemRender a, MemRender b) => MemRender (a :- b) where
    render _ = do
        sa <- render (Proxy :: Proxy a)
        sb <- render (Proxy :: Proxy b)
        return $ sa ++ "-" ++ sb
    parser _ = do
        parser (Proxy :: Proxy a)
        parser (Proxy :: Proxy b)
        

instance (MemRender a, MemRender b) => MemRender (a :| b) where
    render _ = do
        b <- getBit
        if b
            then render (Proxy :: Proxy a)
            else render (Proxy :: Proxy b)
    parser _ = do
        s <- get
        catchError (do
            addBits (putBit True)
            parser (Proxy :: Proxy a) 
            ) (\_ -> do
            put s
            addBits (putBit False)
            parser (Proxy :: Proxy b)
            )

instance (NumberRender nt, KnownNat a, KnownNat o) => MemRender (NumberWithOffset nt a o) where
    render _ = do
        let
            o = natVal (Proxy :: Proxy o)
            b = natVal (Proxy :: Proxy a)
        w <- getBitsFrom (fromIntegral (pred b)) 0
        return $ renderNumber (Proxy :: Proxy nt) b (w + o)

    parser _ = do
        let
            o = natVal (Proxy :: Proxy o)
            b = natVal (Proxy :: Proxy a)
        (ss,cs) <- get
        case ss of
            [] -> empty
            (s:ss') -> do
                let
                    n = readNumber (Proxy :: Proxy nt) b s
                case n of
                    Nothing -> empty
                    Just n' -> do
                        let n'' = n' - o
                        when (n'' >= 2^b) empty
                        put (ss',cs >> putBitsFrom (fromIntegral $ pred b) n'')
                

instance (MemRender a, Depth a <= n, NumberRender nt, KnownNat n, KnownNat (Depth a)) => MemRender (PadTo nt n a) where
    render _ = do
        s1 <- render (Proxy :: Proxy a)
        let
            diff = natVal (Proxy :: Proxy n) - natVal (Proxy :: Proxy (Depth a))
            ntp = Proxy :: Proxy nt
        case diff of
            0 -> return s1
            _ -> do
                d <- getBitsFrom (fromIntegral (pred diff)) 0
                return $ s1 ++ "-" ++ renderNumber ntp diff d

    parser _ = do
        let
            nt = Proxy :: Proxy nt
            diff = natVal (Proxy :: Proxy n) - natVal (Proxy :: Proxy (Depth a))
        parser (Proxy :: Proxy a)
        case diff of
            0 -> return ()
            _ -> do
                (ss,cs) <- get
                when (null ss) empty
                let
                    (s:ss') = ss
                    n = readNumber nt diff s
                n' <- maybe empty return n
                when (n' >= 2^diff) empty
                put (ss', cs >> putBitsFrom (fromIntegral $ pred diff) n')

---------------------------------------------------------------------
-- NumberRender
---------------------------------------------------------------------

-- | Class for capturing how to render numbers.
class NumberRender n where
    renderNumber :: Proxy n -> Integer -> Integer -> String
    readNumber :: Proxy n -> Integer -> String -> Maybe Integer


-- | Render numbers as decimal numbers. Does not pad.
data Dec

instance NumberRender Dec where
    renderNumber _ _ = show
    readNumber _ _ input = case readDec input of
        [(v,"")] -> Just v
        _ -> Nothing


-- | Render numbers as hexadecimal numbers. Pads with 0s.
data Hex

instance NumberRender Hex where
    renderNumber _ b = printf "%0*x" hexDigits
        where
            hexDigits = (b - 1) `div` 4 + 1
    readNumber _ _ input = case readHex input of
        [(v,"")] -> Just v
        _ -> Nothing

---------------------------------------------------------------------
-- Rendering functions for users
---------------------------------------------------------------------

-- | Class for all things that can be converted to memorable strings.
-- See `renderMemorable` for how to use.
class Memorable a where
    -- Do not lie. Use @`testMemLen`@.
    type MemLen a :: Nat
    renderMem :: MonadPut m => a -> Coding m ()
    parserMem :: MonadGet m => Coding m a

memBitSize :: forall a. (KnownNat (MemLen a)) => Proxy a -> Int
memBitSize _ = fromIntegral $ natVal (Proxy :: Proxy (MemLen a))

-- | Use this with tasty-quickcheck (or your prefered testing framework) to
-- make sure you aren't lying about `MemLen`.
-- 
-- @
-- testProperty "MemLen Word8" $ forAll (arbitrary :: Gen Word8) `testMemLen`
-- @
testMemLen :: forall a. (KnownNat (MemLen a), Memorable a) => a -> Bool
testMemLen a =
    let
        p :: Coding PutM ()
        p = renderMem a
        (x,bs) = runPutM (runCoding p (\a x _ -> return x) 0 0)
        l = fromIntegral $ natVal (Proxy :: Proxy (MemLen a))
        bl = 8 * fromIntegral (BL.length bs) - x
    in
        l == bl

putUnaligned :: (MonadPut m, FiniteBits b) => b -> Coding m ()
putUnaligned b = putBitsFrom (pred $ finiteBitSize b) b

instance Memorable Word8 where
    type MemLen Word8 = 8
    renderMem = putUnaligned
    parserMem = getBitsFrom (pred $ memBitSize (Proxy :: Proxy Word8)) 0

instance Memorable Word16 where
    type MemLen Word16 = 16
    renderMem = putUnaligned
    parserMem = getBitsFrom (pred $ memBitSize (Proxy :: Proxy Word16)) 0

instance Memorable Word32 where
    type MemLen Word32 = 32
    renderMem = putUnaligned
    parserMem = getBitsFrom (pred $ memBitSize (Proxy :: Proxy Word32)) 0

instance Memorable Word64 where
    type MemLen Word64 = 64
    renderMem = putUnaligned
    parserMem = getBitsFrom (pred $ memBitSize (Proxy :: Proxy Word64)) 0

instance Memorable Int8 where
    type MemLen Int8 = 8
    renderMem =  putUnaligned
    parserMem = getBitsFrom (pred $ memBitSize (Proxy :: Proxy Int8)) 0

instance Memorable Int16 where
    type MemLen Int16 = 16
    renderMem = putUnaligned
    parserMem = getBitsFrom (pred $ memBitSize (Proxy :: Proxy Int16)) 0

instance Memorable Int32 where
    type MemLen Int32 = 32
    renderMem = putUnaligned
    parserMem = getBitsFrom (pred $ memBitSize (Proxy :: Proxy Int32)) 0

instance Memorable Int64 where
    type MemLen Int64 = 64
    renderMem = putUnaligned
    parserMem = getBitsFrom (pred $ memBitSize (Proxy :: Proxy Int64)) 0

instance (Memorable a, Memorable b) => Memorable (a,b) where
    type MemLen (a,b) = MemLen a + MemLen b
    renderMem (a,b) = renderMem a >> renderMem b
    parserMem = (,) <$> parserMem <*> parserMem

instance (Memorable a, Memorable b, Memorable c) => Memorable (a,b,c) where
    type MemLen (a,b,c) = MemLen a + MemLen b + MemLen c
    renderMem (a,b,c) = renderMem a >> renderMem b >> renderMem c
    parserMem = (,,) <$> parserMem <*> parserMem <*> parserMem


instance (Memorable a, Memorable b, Memorable c, Memorable d) => Memorable (a,b,c,d) where
    type MemLen (a,b,c,d) = MemLen a + MemLen b + MemLen c + MemLen d
    renderMem (a,b,c,d) = renderMem a >> renderMem b >> renderMem c >> renderMem d
    parserMem = (,,,) <$> parserMem <*> parserMem <*> parserMem <*> parserMem

instance (Memorable a, Memorable b, Memorable c, Memorable d, Memorable e) => Memorable (a,b,c,d,e) where
    type MemLen (a,b,c,d,e) = MemLen a + MemLen b + MemLen c + MemLen d + MemLen e
    renderMem (a,b,c,d,e) = renderMem a >> renderMem b >> renderMem c >> renderMem d >> renderMem e
    parserMem = (,,,,) <$> parserMem <*> parserMem <*> parserMem <*> parserMem <*> parserMem

#ifdef DATA_DWORD
instance Memorable Word96 where
    type MemLen Word96 = 96
    renderMem (Word96 h l) = renderMem h >> renderMem l
    parserMem = Word96 <$> parserMem <*> parserMem

instance Memorable Word128 where
    type MemLen Word128 = 128
    renderMem (Word128 h l) = renderMem h >> renderMem l
    parserMem = Word128 <$> parserMem <*> parserMem

instance Memorable Word160 where
    type MemLen Word160 = 160
    renderMem (Word160 h l) = renderMem h >> renderMem l
    parserMem = Word160 <$> parserMem <*> parserMem

instance Memorable Word192 where
    type MemLen Word192 = 192
    renderMem (Word192 h l) = renderMem h >> renderMem l
    parserMem = Word192 <$> parserMem <*> parserMem

instance Memorable Word224 where
    type MemLen Word224 = 224
    renderMem (Word224 h l) = renderMem h >> renderMem l
    parserMem = Word224 <$> parserMem <*> parserMem

instance Memorable Word256 where
    type MemLen Word256 = 256
    renderMem (Word256 h l) = renderMem h >> renderMem l
    parserMem = Word256 <$> parserMem <*> parserMem

instance Memorable Int96 where
    type MemLen Int96 = 96
    renderMem (Int96 h l) = renderMem h >> renderMem l
    parserMem = Int96 <$> parserMem <*> parserMem

instance Memorable Int128 where
    type MemLen Int128 = 128
    renderMem (Int128 h l) = renderMem h >> renderMem l
    parserMem = Int128 <$> parserMem <*> parserMem

instance Memorable Int160 where
    type MemLen Int160 = 160
    renderMem (Int160 h l) = renderMem h >> renderMem l
    parserMem = Int160 <$> parserMem <*> parserMem

instance Memorable Int192 where
    type MemLen Int192 = 192
    renderMem (Int192 h l) = renderMem h >> renderMem l
    parserMem = Int192 <$> parserMem <*> parserMem

instance Memorable Int224 where
    type MemLen Int224 = 224
    renderMem (Int224 h l) = renderMem h >> renderMem l
    parserMem = Int224 <$> parserMem <*> parserMem

instance Memorable Int256 where
    type MemLen Int256 = 256
    renderMem (Int256 h l) = renderMem h >> renderMem l
    parserMem = Int256 <$> parserMem <*> parserMem
#endif

#ifdef NETWORK_IP
-- | >>> renderMemorable threeWordsFor32Bits (ip4FromOctets 127 0 0 1)
-- "shore-pick-pets"
instance Memorable IP4 where
    type MemLen IP4 = 32
    renderMem (IP4 w) = renderMem w
    parserMem = IP4 <$> parserMem

instance Memorable IP6 where
    type MemLen IP6 = 128
    renderMem (IP6 w) = renderMem w
    parserMem = IP6 <$> parserMem
#endif

#ifdef CRYPTONITE
#define DIGEST_INST(NAME,BITS) \
instance Memorable (Digest NAME) where {\
    type MemLen (Digest NAME) = BITS; \
    renderMem = mapM_ putUnaligned . B.unpack . convert; \
    parserMem = do { \
        let {b = (BITS) `div` 8;}; \
        fromJust <$> (digestFromByteString . B.pack) <$> replicateM b (getBitsFrom 7 0); \
        }}

DIGEST_INST(Whirlpool,512)
DIGEST_INST(Blake2s_224,224)
DIGEST_INST(Blake2s_256,256)
DIGEST_INST(Blake2sp_224,224)
DIGEST_INST(Blake2sp_256,256)
DIGEST_INST(Blake2b_512,512)
DIGEST_INST(Blake2bp_512,512)
DIGEST_INST(Tiger,192)
DIGEST_INST(Skein512_512,512)
DIGEST_INST(Skein512_384,384)
DIGEST_INST(Skein512_256,256)
DIGEST_INST(Skein512_224,224)
DIGEST_INST(Skein256_224,224)
DIGEST_INST(Skein256_256,256)
DIGEST_INST(SHA512t_256,256)
DIGEST_INST(SHA512t_224,224)
DIGEST_INST(SHA512,512)
DIGEST_INST(SHA384,384)
DIGEST_INST(SHA3_512,512)
DIGEST_INST(SHA3_384,384)
DIGEST_INST(SHA3_256,256)
DIGEST_INST(SHA3_224,224)
DIGEST_INST(SHA256,256)
DIGEST_INST(SHA224,224)
DIGEST_INST(SHA1,160)
DIGEST_INST(RIPEMD160,160)
-- | 
-- >>> :set -XOverloadedStrings
-- >>> import Data.ByteString
-- >>> import Crypto.Hash
-- >>> let myPattern = padHex (four flw10)
-- >>> renderMemorable myPattern (hash ("anExample" :: ByteString) :: Digest MD5)
-- "bark-most-gush-tuft-1b7155ab0dce3ecb4195fc"
DIGEST_INST(MD5,128)
DIGEST_INST(MD4,128)
DIGEST_INST(MD2,128)
DIGEST_INST(Keccak_512,512)
DIGEST_INST(Keccak_384,384)
DIGEST_INST(Keccak_256,256)
DIGEST_INST(Keccak_224,224)
#undef DIGEST_INST
#endif

-- | This is the function to use when you want to turn your values into a
-- memorable strings.
--
-- >>> import Data.Word
-- >>> import Data.Memorable.Theme.Words
-- >>> let myPattern = words8 .- words8
-- >>> renderMemorable myPattern (0x0123 :: Word16)
-- "cats-bulk"
renderMemorable :: (MemRender p, Depth p ~ MemLen a, Memorable a) => Proxy p -> a -> String
renderMemorable p a = renderMemorableByteString p (runRender a)

runRender :: Memorable a => a -> ByteString
runRender c = runPutL (runCoding (renderMem c) (\_ _ _ -> pure ()) 0 0)

runParser :: Memorable a => ByteString -> a
runParser = runGet (runCoding parserMem (\a _ _ -> pure a) 0 0)

-- | Render a `ByteString` as a more memorable `String`.
renderMemorableByteString
    :: MemRender a
    => Proxy a -> ByteString -> String
renderMemorableByteString p =
    runGetL (runCoding (render p) (\a _ _ -> return a) 0 0)
    --runGet (runBitGet . flip evalStateT 0 . unBitPull $ render p)

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


-- | Render any `Hashable` value as a 32 bit pattern.
renderHashable32 :: (MemRender p, Depth p ~ 32, Hashable a) => Proxy p -> a -> String
renderHashable32 p a = renderMemorable p (fromIntegral $ hash a :: Word32)

-- | Render any `Hashable` value as a 16 bit pattern.
renderHashable16 :: (MemRender p, Depth p ~ 16, Hashable a) => Proxy p -> a -> String
renderHashable16 p a = renderMemorable p (fromIntegral $ hash a :: Word16)

-- | Render any `Hashable` value as a 8 bit pattern.
renderHashable8 :: (MemRender p, Depth p ~ 8, Hashable a) => Proxy p -> a -> String
renderHashable8 p a = renderMemorable p (fromIntegral $ hash a :: Word8)

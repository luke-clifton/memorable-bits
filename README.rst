================
 Memorable Bits
================

A library for generating human-readable (and memorable) phrases from
data. What sets this library apart from most others, is that it is highly
customisable. The phrases that are generated are described at the type
level, which can ensure, at compile time, that the phrase generated has
enough bits to cover the data being converted.

The library comes with some pre-baked themes::

    ghci> :set -XOverloadedStrings
    ghci> import Data.Memorable
    ghci>
    ghci> import Data.Memorable.Theme.Fantasy
    ghci> renderMemorable (padHex rpgWeapons) (12345 :: Word32)
    "ghostly-trident-of-zombie-tormenting-039"
    ghci>
    ghci> renderMemorable (padHex desserts) (12345 :: Word32)
    "creamy-cherry-cupcakes-003039"
    ghci>
    ghci> import Data.Memorable.Theme.Words
    ghci> renderMemorable threeWordsFor32Bits (12345 :: Word32)
    "colleague-peas-omit"
    ghci>
    ghci> renderMemorable fourEqualWordsFor32Bits (12345 :: Word32)
    "chap-chap-iris-rate"
    ghci>
    ghci> import Data.Memorable.Theme.Science
    ghci> renderMemorable (padDec chemBabble) (0xdeadbeef :: Word32)
    "tri-propyl-4-benzene-oxide-114415"

But you can easily create your own and mix and match::

    ghci> :set -XDataKinds -XTypeOperators
    ghci> import Data.Memorable.Theme.Fantasy as Fantasy
    ghci> import Data.Memorable.Theme.Food as Food
    ghci> type MyPattern = NumberWithOffset Dec 1 3 :- Food.DessertFlavours :- Fantasy.Monster
    ghci> let myPattern = Proxy :: Proxy MyPattern
    ghci> renderMemorable (padHex myPatter) (0x4321 :: Word16)
    "3-blueberry-sphinx-01"

You can check how many bits are covered by your pattern::

    ghci> :kind! Depth MyPattern
    Depth MyPattern :: Nat
    = 7

Or using the ``getDepth`` function::

    ghci> getDepth words12
    12
    ghci> getDepth (words12 .- words12)
    24

If you are happy to have some non-human-readable parts in your pattern, use
the convenience functions ``padHex`` and ``padDec``::

    ghci> :set -XOverloadedStrings
    ghci> import Data.ByteString
    ghci> import Crypto.Hash
    ghci> let myPattern = padHex (four flw10)
    ghci> renderMemorable myPattern (hash ("anExample" :: ByteString) :: Digest MD5)
    "bark-most-gush-tuft-1b7155ab0dce3ecb4195fc"


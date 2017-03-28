================================
 Memorable Bits |hackage-badge|
================================

.. |hackage-badge| image:: https://img.shields.io/hackage/v/memorable-bits.svg


A library for generating human-readable (and memorable) phrases from
data. What sets this library apart from most others, is that it is highly
customisable. The phrases that are generated are described at the type
level, which can ensure, at compile time, that the phrase generated has
enough bits to cover the data being converted.

The library comes with some pre-baked themes::

    λ> :set -XOverloadedStrings
    λ> import Data.Word
    λ> import Crypto.Hash
    λ> import Network.IP.Addr
    λ> import Data.Memorable
    λ>
    λ> import Data.Memorable.Theme.Fantasy
    λ> renderMemorable (padHex rpgWeapons) (12345 :: Word32)
    "ghostly-trident-of-zombie-tormenting-039"
    λ>
    λ> import Data.Memorable.Theme.Food
    λ> renderMemorable (padHex desserts) $ ip4FromOctets 127 0 0 1
    "bland-strawberry-pancakes-000001"
    λ>
    λ> import Data.Memorable.Theme.Words
    λ> let h = hash ("an example" :: ByteString) :: Digest MD5
    λ> renderMemorable (padHex eightEqualWordsFor64Bits) a
    "than-high-bonk-gash-into-keen-rush-swat-7efea731142d8f84"
    λ>
    λ> import Data.Memorable.Theme.Science
    λ> renderMemorable (padDec chemBabble) (0xdeadbeef :: Word32)
    "tri-propyl-4-benzene-oxide-114415"

But you can easily create your own and mix and match::

    λ> :set -XDataKinds -XTypeOperators
    λ> import Data.Memorable.Theme.Fantasy as Fantasy
    λ> import Data.Memorable.Theme.Food as Food
    λ> type MyPattern = NumberWithOffset Dec 1 3 :- Food.DessertFlavours :- Fantasy.Monster
    λ> let myPattern = Proxy :: Proxy MyPattern
    λ> renderMemorable (padHex myPatter) (0x4321 :: Word16)
    "3-blueberry-sphinx-01"

You can check how many bits are covered by your pattern::

    λ> :kind! Depth MyPattern
    Depth MyPattern :: Nat
    = 7

Or using the ``getDepth`` function::

    λ> getDepth words12
    12
    λ> getDepth (words12 .- words12)
    24

If you are happy to have some non-human-readable parts in your pattern, use
the convenience functions ``padHex`` and ``padDec``::

    λ> :set -XOverloadedStrings
    λ> import Data.ByteString
    λ> import Crypto.Hash
    λ> let myPattern = padHex (four flw10)
    λ> renderMemorable myPattern (hash ("anExample" :: ByteString) :: Digest MD5)
    "bark-most-gush-tuft-1b7155ab0dce3ecb4195fc"


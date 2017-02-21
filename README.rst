================
 Memorable Bits
================

A library for generating human-readable (and memorable) phrases from
data. What sets this library apart from most others, is that it is highly
customisable. The phrases that are generated are described at the type
level, which can ensure, at compile time, that the phrase generated has
enough bits to cover the data being converted (though, you can opt out, and
just live with a prefix if you want).

The library comes with some pre-baked themes:

    ghci> :set -XOverloadedStrings
    ghci> import Data.Memorable
    ghci>
    ghci> import Data.Memorable.Theme.Fantasy
    ghci> renderMemorable rpgThings "memorable"
    "sacred sword of gnoll poking"
    ghci>
    ghci> import Data.Memorable.Theme.Food
    ghci> renderMemorable desserts "memorable"
    "gourmet-chocolate-souffle"
    ghci>
    ghci> import Data.Memorable.Theme.Words
    ghci> renderMemorable fourWords "memorable"
    "count-reform-waters-discussions"
    ghci>
    ghci> import Data.Memorable.Theme.Science
    ghci> renderMemorable chemBabble "memorable"
    "6-methyl-9-ethyl-sulfide"

But you can easily create your own and mix and match:

    ghci> :set -XDataKinds
    ghci> import Data.Memorable.Theme.Fantasy as Fantasy
    ghci> import Data.Memorable.Theme.Food as Food
    ghci> type MyPattern = NumberWithOffset Dec 1 3 :<-> Food.DessertFlavours :<-> Fantasy.Monster :<> "s"
    ghci> let myPattern = Proxy :: Proxy MyPattern
    ghci> renderMemorable myPattern "memorable"
    "3-chocolate-goblins"

You can check how many bits are covered by your pattern:

    ghci> :kind! MinBits MyPattern
    MinBits MyPattern :: Nat
    = 7
    ghci> :kind! MaxBits MyPattern
    MaxBits MyPattern :: Nat
    = 7

Here our pattern consumes 7 bits, regardless of which path through the pattern
it takes. This is can be required by using the ``ConstantSize`` or ``ExactSize``
constraint on a pattern. It is fairly important to make sure your pattern
contains enough bits to be useful for your application. In GHCi, use the
same trick as above to explore your options:

    ghci> :kind! MinBits Words
    MinBits Words :: Nat
    = 12
    ghci> :kind! MinBits (Words :<-> Words)
    MinBits (Words :<-> Words) :: Nat
    = 24

If you really need each string to cover the entire input range, and are happy
to have some non-human readable parts, use ``PadTo``

    ghci> :kind! MinBits (PadTo "-" Hex 512 Words)
    MinBits (PadTo "-" Hex 72 Words) :: Nat
    = 72
    ghci> renderMemorable (Proxy :: Proxy (PadTo "-" Hex 72 Words)) "memorable"
    "count-a63646864ef6b6a"



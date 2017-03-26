{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception
import Crypto.Hash
import qualified Data.ByteString as B
import Data.Char
import Data.Memorable
import Data.Memorable.Theme.Words
import GHC.TypeLits
import Options.Applicative
import System.Environment
import System.IO

data Opt = Opt
    { algorithm :: FilePath -> IO ()
    , files :: [FilePath]
    }

algOption ::
    ( Show a
    , 40 <= MemLen (Digest a)
    , KnownNat (MemLen (Digest a))
    , Memorable (Digest a)
    , HashAlgorithm a
    ) => a -> Parser (FilePath -> IO ())
algOption a = flag' (run a) (long (map toLower $ show a))

algs :: [Parser (FilePath -> IO ())]
algs =
    [ algOption Blake2b_512
    , algOption Blake2bp_512
    , algOption Blake2s_224
    , algOption Blake2s_256
    , algOption Blake2sp_224
    , algOption Blake2sp_256
    , algOption Keccak_224
    , algOption Keccak_256
    , algOption Keccak_384
    , algOption Keccak_512
    , algOption MD2
    , algOption MD4
    , algOption MD5
    , algOption RIPEMD160
    , algOption SHA1
    , algOption SHA224
    , algOption SHA256
    , algOption SHA384
    , algOption SHA3_224
    , algOption SHA3_256
    , algOption SHA3_384
    , algOption SHA3_512
    , algOption SHA512
    , algOption SHA512t_224
    , algOption SHA512t_256
    , algOption Skein256_224
    , algOption Skein256_256
    , algOption Skein512_224
    , algOption Skein512_256
    , algOption Skein512_384
    , algOption Skein512_512
    , algOption Tiger
    , algOption Whirlpool
    ]

choice :: Alternative f => f a -> [f a] -> f a
choice = foldr (<|>)

opts :: Parser Opt
opts = do
    algorithm <- parseAlgorithm
    files <- parseFiles
    pure Opt{..}

    where
        parseAlgorithm = choice empty algs
        parseFiles = many $ strArgument (metavar "FILE")

main :: IO ()
main = do
    Opt{..} <- execParser (info opts fullDesc)
    mapM_ algorithm files

run :: forall a.
    ( KnownNat (MemLen (Digest a))
    , Memorable (Digest a)
    , 40 <= MemLen (Digest a)
    , HashAlgorithm a
    ) => a -> FilePath -> IO ()
run a f = do
    res <- try $ do
        bs <- B.readFile f
        let
            h = hash bs :: Digest a
        return h
    case res of
        Right h -> do
            putStr $ renderMemorable (padHex $ four flw10) h
            putStr "\t"
            putStrLn f
        Left (e :: SomeException) -> hPutStrLn stderr (displayException e)

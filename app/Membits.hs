{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Unsafe.Coerce
import Control.Exception
import Crypto.Hash
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Maybe
import Data.Memorable
import Data.Memorable.Theme.Words
import Data.Monoid
import Data.Type.Equality
import GHC.TypeLits
import Options.Applicative
import System.Environment
import System.IO

type Algorithm = B.ByteString -> String

data Opt
    = HashOpt
        { algorithm :: Algorithm
        , files :: [FilePath]
        }
    | Convert

algOption ::
    ( Show a
    , 40 <= MemLen (Digest a)
    , KnownNat (MemLen (Digest a))
    , Memorable (Digest a)
    , HashAlgorithm a
    ) => a -> Parser Algorithm
algOption a = flag' (goAlg a) (long (map toLower $ show a))

algs :: [Parser Algorithm]
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
opts = parseHashOpts <|> parseConv
    where
        parseHashOpts = do
            algorithm <- parseAlgorithm
            files <- parseFiles
            pure HashOpt{..}
        parseAlgorithm = choice empty algs
        parseFiles = many $ strArgument (metavar "FILE" <> help "Files to hash" <> action "file")
        parseConv = flag' Convert (long "conv" <> help "Convert hex lines on stdin to a memorable pattern")

main :: IO ()
main = do
    opts <- execParser (info opts fullDesc)
    case opts of
        Convert -> do
            ls <- lines <$> getContents
            mapM_ (either (hPutStrLn stderr) putStrLn . doConv) ls
        HashOpt{..} -> mapM_ (run algorithm) files

run :: Algorithm -> FilePath -> IO ()
run a f = do
    res <- try $ do
        bs <- B.readFile f
        return $! a bs
    case res of
        Right h -> do
            putStr h
            putStr "\t"
            putStrLn f
        Left (e :: SomeException) -> hPutStrLn stderr (displayException e)

goAlg :: forall a.
    ( 40 <= MemLen (Digest a)
    , KnownNat (MemLen (Digest a))
    , Memorable (Digest a)
    , HashAlgorithm a
    ) => a -> Algorithm
goAlg a b =
    let
        h = hashlazy b :: Digest a
    in
        renderMemorable (padHex $ four flw10) h

moreThan40 :: KnownNat n => Proxy (n :: Nat) -> Maybe ((40 <=? n) :~: True)
moreThan40 p = if natVal p > 40 then Just $ unsafeCoerce Refl else Nothing

doConv :: String -> Either String String
doConv i =
    let
        Just bitsIn = someNatVal $ fromIntegral $ length i * 4
    in
        case bitsIn of
            SomeNat (p :: Proxy n) ->
                case moreThan40 p of
                    Just Refl -> 
                        let
                            s = rerender (hex @n) (padHex $ four flw10) i
                        in
                            maybe (Left $ "failed to convert '" ++ i ++ "'") Right s
                    Nothing -> Left $ "failed to convert '" ++ i ++ "'"


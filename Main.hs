{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Simple matrix routing API.
Copyright   : Travis Whitaker 2015
License     : MIT
Maintainer  : twhitak@its.jnj.com
Stability   : Provisional
Portability : POSIX
-}

module Main where

import Control.Monad (liftM2)

import Data.Word (Word8)
import Data.Char (ord)
import Data.List (transpose)
import Data.Maybe (listToMaybe)
import Data.Aeson.Types (ToJSON(toJSON), Value(Array, String))

import Web.Scotty (Parsable(parseParam), scotty, get, json, param)

import System.Environment (getArgs)

import qualified Data.ByteString.Lazy    as B  (ByteString, toStrict, readFile, split)
import qualified Data.Text               as T  (pack)
import qualified Data.Text.Encoding      as T  (decodeUtf8)
import qualified Data.Text.Lazy.Encoding as TL (encodeUtf8)
import qualified Data.Map                as M  (Map, fromList, keys, lookup)
import qualified Data.Vector             as V  (Vector, fromList, length, slice, map)

-- | Data block name.
type MAlias   = String

-- | Data block column name.
type MHeader  = B.ByteString

instance Parsable B.ByteString where
    parseParam = Right . TL.encodeUtf8

-- | Data block cell content.
type MCell    = B.ByteString

-- | Data block represented as a map from column headers to a vector of column
--   contents.
type MContent = M.Map MHeader (V.Vector MCell)

-- | Global server state represented as a map from available data block names
--   to data block contents.
type MState   = M.Map MAlias MContent

-- | A slice of a data block column.
newtype MSlice = MSlice {unMSlice :: (Int, Int)}

instance Eq MSlice where
    (==) = (. unMSlice) . (==) . unMSlice

instance Show MSlice where
    show = ("MSlice " ++) . show . unMSlice

-- | Client request representation type.
data MReq = MReqAliases        |                       -- ^ Request for available aliases.
            MReqHeaders MAlias |                       -- ^ Request for available headers.
            MReqContents MAlias MHeader (Maybe MSlice) -- ^ Request for a column vector.
            deriving (Eq, Show)

-- | Server response type.
data MRes = MResAliases [MAlias]          | -- ^ Alias list.
            MResHeaders [MHeader]         | -- ^ Header list.
            MResContents (V.Vector MCell) | -- ^ Column vector slice.
            MResError String                -- ^ Request error.
            deriving (Eq, Show)

instance ToJSON MRes where
    toJSON (MResAliases  as) = Array $ V.fromList (map (String . T.pack) as)
    toJSON (MResHeaders  hs) = Array $ V.fromList (map (String . T.decodeUtf8 . B.toStrict) hs)
    toJSON (MResContents cs) = Array $ V.map (String . T.decodeUtf8 . B.toStrict) cs
    toJSON (MResError err)   = toJSON err

toWord8 :: Char -> Word8
toWord8 = fromIntegral . ord

nord = toWord8 '\n'
cord = toWord8 ','

-- | Construct an 'MSlice' from a client slice parameter string.
mslice :: String -> Maybe MSlice
mslice p = (readMaybe ("(" ++ x ++ "," ++ (check y) ++ ")")) >>= (return . MSlice)
    where readMaybe  = fmap fst . listToMaybe . reads
          (x, y) = break (== '.') p
          check (_:a) = a
          check _     = []

-- | Build the global server state from a list of static data blocks to serve.
getMState :: [FilePath] -> IO MState
getMState = (makeState `fmap`) . liftM2 fmap zip (mapM B.readFile)
    where makeState = M.fromList . map makeContent
          makeContent (a, bs) = (a, (M.fromList . makeVector . makeCells) bs)
          makeVector = map (\(x:xs) -> (x, V.fromList xs))
          makeCells = transpose . map (B.split cord) . (B.split nord)

-- | Service an 'MReq' from an 'MState'.
serviceMReq :: MState -> MReq -> MRes
serviceMReq st MReqAliases     = MResAliases $ M.keys st
serviceMReq st (MReqHeaders a) = case M.lookup a st of Just hs -> MResHeaders $ M.keys hs
                                                       Nothing -> MResError $ "MAlias \"" ++ a ++ "\" does not exist."
serviceMReq st (MReqContents a h Nothing) = case (M.lookup a st) >>= (M.lookup h)
    of Just v  -> MResContents $ v
       Nothing -> MResError $ "Binary tree lookup error."
serviceMReq st (MReqContents a h (Just (MSlice (x, y)))) = case (M.lookup a st) >>= (M.lookup h)
    of Just v  -> if (x + y) <= (V.length v) then (MResContents (V.slice x y v)) else (MResError "Index error.")
       Nothing -> MResError $ "Binary tree lookup error."

-- | Serve an 'MState' on a given port number.
renderMState :: Int -> MState -> IO ()
renderMState p st = scotty p r
    where r = get "/a" (json $ serviceMReq st MReqAliases) >>
              get "/h/:a" (param "a" >>= (\a -> json $ serviceMReq st (MReqHeaders a))) >>
              get "/c/:a/:h" (param "a" >>= (\a ->
                             (param "h" >>= (\h ->
                             (json $ serviceMReq st (MReqContents a h Nothing)))))) >>
              get "/c/:a/:h/:s" (param "a" >>= (\a ->
                                (param "h" >>= (\h ->
                                (param "s" >>= (\s ->
                                (json $ serviceMReq st (MReqContents a h (mslice s)))))))))

-- | Validate command line arguments.
parseArgs :: [String] -> IO ()
parseArgs (p : f : fs) = getMState (f:fs) >>= renderMState (read p)
parseArgs _            = putStrLn "Usage:\nMRoute port_number file1 file2 ..."

main :: IO ()
main = getArgs >>= parseArgs

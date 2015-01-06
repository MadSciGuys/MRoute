{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Monoid

import Data.Word
import Data.Char
import Data.List
import Data.Maybe
import Data.Aeson.Types

import Web.Scotty

import System.Environment

import qualified Data.ByteString.Lazy    as B
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Map                as M
import qualified Data.Vector             as V

type MAlias   = String
type MHeader  = B.ByteString
type MCell    = B.ByteString
type MContent = M.Map MHeader (V.Vector MCell)
type MState   = M.Map MAlias MContent

newtype MSlice = MSlice {unMSlice :: (Int, Int)}

instance Eq MSlice where
    (==) = (. unMSlice) . (==) . unMSlice

instance Show MSlice where
    show = ("MSlice " ++) . show . unMSlice

instance Parsable B.ByteString where
    parseParam = Right . TL.encodeUtf8

data MReq = MReqAliases |
            MReqHeaders MAlias |
            MReqContents MAlias MHeader (Maybe MSlice)
            deriving (Eq, Show)

data MRes = MResAliases [MAlias] |
            MResHeaders [MHeader] |
            MResContents (V.Vector MCell) |
            MResError String
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

mslice :: String -> Maybe MSlice
mslice p = (readMaybe ("(" ++ x ++ "," ++ (check y) ++ ")")) >>= (return . MSlice)
    where readMaybe  = fmap fst . listToMaybe . reads
          (x, y) = break (== '.') p
          check (_:a) = a
          check _     = []

getMState :: [FilePath] -> IO MState
getMState = (makeState `fmap`) . liftM2 fmap zip (mapM B.readFile)
    where makeState = M.fromList . map makeContent
          makeContent (a, bs) = (a, (M.fromList . makeVector . makeCells) bs)
          makeVector = map (\(x:xs) -> (x, V.fromList xs))
          makeCells = transpose . map (B.split cord) . (B.split nord)

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

parseArgs :: [String] -> IO ()
parseArgs (p : f : fs) = getMState (f:fs) >>= renderMState (read p)
parseArgs _            = putStrLn "Usage:\nMRoute port_number file1 file2 ..."

main :: IO ()
main = getArgs >>= parseArgs

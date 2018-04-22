module Adapter where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.List (sortBy)
import Data.Map (Map, fromList, toList)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Formatter (Formatter, OutputFormat)

-- 前のモジュールの出力jsonを次のモジュールの入力json形式に変換する
adapter :: Formatter -> String -> String
adapter formatter output =
    let
        outputM = decode . str2b $ output :: Maybe OutputFormat
    in
        case outputM of
            Nothing -> error "invalid output format: \"" ++ output ++ "\""
            Just o -> b2str . encode . convertKey o $ formatter

convertKey :: (Ord k, Ord l) => Map k a -> Map k l -> Map l a
convertKey m keyMap =
    let
        mL = sortBy (\x y -> compare (fst x) (fst y)) $ toList m
        keyL = sortBy (\x y -> compare (fst x) (fst y)) $ toList keyMap
        xs = zip mL keyL
    in
        fromList [(m,v) | ((k,v),(l,m)) <- xs]

str2b :: String -> ByteString
str2b = encodeUtf8 . pack

b2str :: ByteString -> String
b2str = unpack . decodeUtf8

decodeJStr2Str :: String -> String
decodeJStr2Str s = b2str . encode $ jsonMap
    where
        s' = decode . str2b $ s :: Maybe (Map String String)
        jsonMap = fromMaybe (fromList [("output","invalid json format!!")]) s'

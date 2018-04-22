module Formatter where

import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import Data.List (sortBy)
import Data.Map.Strict (elems, fromList, keys, toList, Map)
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B

type OutputFormat = Map String String
type InputFormat = Map String String
type Formatter = Map String String

-- 前のモジュールからのjson出力を次のモジュールのjson入力に接続するための対応づけルールを作成
-- 今の所、jsonキーの対応づけのみで、型変換はしない
mkFormatter :: OutputFormat -> InputFormat -> IO Formatter
mkFormatter outFormat inFormat
    | outFormat == inFormat             = return $ mkFormatterIdentity outFormat inFormat
    | isSameTypeEach outFormat inFormat = return $ mkFormatterSameTypeEach outFormat inFormat
    | otherwise                         = mkFormatterInteractive outFormat inFormat

mkFormatterIdentity :: OutputFormat -> InputFormat -> Formatter
mkFormatterIdentity outFormat inFormat = fromList $ zip (keys outFormat) (keys outFormat)

isSameTypeEach :: OutputFormat -> InputFormat -> Bool
isSameTypeEach outFormat inFormat =
    let
        noDuplicatedOutTypes = length (elems outFormat) == length (S.fromList $ elems outFormat)
        noDuplicatedInTypes = length (elems inFormat) == length (S.fromList $ elems inFormat)
        sameTypeEach = length (elems outFormat) == length (elems inFormat)
    in
        noDuplicatedOutTypes && noDuplicatedInTypes && sameTypeEach

mkFormatterSameTypeEach :: OutputFormat -> InputFormat -> Formatter
mkFormatterSameTypeEach outFormat inFormat =
    let
        outList = sortBy (\(_,a) (_,b) -> compare a b) $ toList outFormat
        inList = sortBy (\(_,a) (_,b) -> compare a b) $ toList inFormat
    in
        fromList $ map (\((a,_),(b,_)) -> (a,b)) (zip outList inList)

mkFormatterInteractive :: OutputFormat -> InputFormat -> IO Formatter
mkFormatterInteractive outFormat inFormat = do
    putStrLn "This method is not implemented yet."
    return $ fromList [("","")]



outFormat1 = fromList [("fileContent","string"), ("num","number")] -- output format of former module
inFormat1 = fromList [("text","string"), ("num","number")] -- input format of this module
inFormat2 = fromList [("text","string"), ("num","number"), ("text2","string")] -- input format of this module

-- example
main :: IO ()
main = do
    f0 <- mkFormatter outFormat1 outFormat1
    putStrLn $ "identity:  " ++ show f0 ++ "\n"
    f1 <- mkFormatter outFormat1 inFormat1
    putStrLn $ "same type each:  " ++ show f1 ++ "\n"
    f2 <- mkFormatter outFormat1 inFormat2
    putStrLn $ "interactive:  " ++ show f2

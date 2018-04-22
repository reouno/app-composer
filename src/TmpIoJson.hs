{-# LANGUAGE DeriveGeneric #-}

module TmpIoJson where

import Data.Aeson (decode, encode, FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (fromList)
import Formatter (InputFormat, OutputFormat)
import GHC.Generics
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B

data TmpIoJson = TmpIoJson
    { input :: InputFormat
    , output :: OutputFormat
    } deriving (Generic, Show)

instance FromJSON TmpIoJson

ioJsonInit = TmpIoJson
    { input = fromList [("arg0", "string")]
    , output = fromList [("out0", "string")]}

readIoJson :: String -> IO TmpIoJson
readIoJson path = do
    text <- B.readFile path
    let ioJson = decode text :: Maybe TmpIoJson
    case ioJson of
        Nothing -> putStrLn ("Cannot read " ++ path ++ ". use default format.") >> return ioJsonInit
        Just ioJson' -> return ioJson'

-- 暫定的な処理
-- 将来的には出力形式も指定できるようにする
addEndIoJson :: [TmpIoJson] -> [TmpIoJson]
addEndIoJson ioJsons = ioJsons ++ [TmpIoJson lastOutput lastOutput]
    where lastOutput = output $ last ioJsons

main :: IO ()
main = do
    arg <- getArgs
    let path = if length arg == 1 && not (null (head arg))
                then head arg
                else "io.json.ini"
    ioJson <- readIoJson path
    print ioJson

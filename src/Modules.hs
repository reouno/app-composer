{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Modules where

import Data.Aeson (decode, encode, FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import System.Console.CmdArgs
import System.Process (readProcessWithExitCode)
import qualified Data.ByteString.Lazy.Char8 as B

readerPath = "/Users/reo/Happy/hap_modules_json/reader/read.py"
wakatiPath = "/Users/reo/Happy/hap_modules_json/morphAnalyzer/wakati.py"

newtype Arguments = Arguments
    { input :: String
    } deriving (Show, Data, Typeable)

arguments = Arguments "no file"

newtype Input = Input
    { filePath :: String
    } deriving (Eq, Generic, Show)

instance FromJSON Input
instance ToJSON Input

newtype ReaderOutput = ReaderOutput
    { fileContent :: String
    } deriving (Eq, Generic, Show)

instance FromJSON ReaderOutput

newtype WakatiInput = WakatiInput
    { text :: String
    } deriving (Eq, Generic, Show)

instance ToJSON WakatiInput

newtype WakatiOutput = WakatiOutput
    { wakati :: String
    } deriving (Eq, Generic, Show)

instance FromJSON WakatiOutput
instance ToJSON WakatiOutput

run :: IO ()
run = do
    inputArgs <- cmdArgs arguments
    let readerInput = decode . str2b . input $ inputArgs :: Maybe Input
    readerOutput <- runReader readerInput
    wakatiOutput <- runWakati readerOutput
    outputJson <- output wakatiOutput
    putStrLn outputJson

main :: IO ()
main = run

str2b :: String -> ByteString
str2b = encodeUtf8 . pack

b2str :: ByteString -> String
b2str = unpack . decodeUtf8

adapter :: ReaderOutput -> WakatiInput
adapter output = WakatiInput (fileContent output)

runReader :: Maybe Input -> IO (Maybe ReaderOutput)
runReader input =
    case input of
        Nothing -> error "invalid input"
        Just i -> do
            let inputJson = b2str . encode $ i
            (status, stdout, stderr) <- readProcessWithExitCode readerPath [inputJson] ""
            return (decode $ str2b stdout :: Maybe ReaderOutput)

runWakati :: Maybe ReaderOutput -> IO (Maybe WakatiOutput)
runWakati readerOutput =
    case readerOutput of
        Nothing -> error "output format of reader is invalid."
        Just output -> do
            let inputJson = b2str . encode . adapter $ output
            (status, stdout, stderr) <- readProcessWithExitCode wakatiPath [inputJson] ""
            return (decode $ str2b stdout :: Maybe WakatiOutput)

output :: Maybe WakatiOutput -> IO String
output wakatiOutput =
    case wakatiOutput of
        Nothing -> error "output format of wakati is invalid."
        Just output -> return $ b2str . encode $ output

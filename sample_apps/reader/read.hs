#!/usr/bin/env stack
-- stack runghc

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import System.Posix.Env.ByteString (getArgs)

data Input = Input {
    filePath :: String
    } deriving (Generic, Show)

data Output = Output {
    fileContent :: String
} deriving (Generic, Show)

instance FromJSON Input
instance ToJSON Output

main :: IO ()
main = do

    -- decode json-formatted args and get file path
    args <- getArgs
    let input = decode . fromStrict . head $ args :: Maybe Input
    text <- case input of
        Just t -> return t
        Nothing -> error "error parsing json"

    -- read file
    content <- readFile $ filePath text

    -- output file content as json format
    let output = encode Output {fileContent = content}
    putStrLn . unpack . decodeUtf8 $ output

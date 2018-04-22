{-# LANGUAGE DeriveDataTypeable #-}

module Run where

import Adapter (decodeJStr2Str, b2str, str2b)
import App (mkApp, run)
import BindType (Bind(..))
import Control.Monad (mapM)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Typeable (typeOf)
import Formatter (Formatter, mkFormatter)
import GraphParser (parse)
import System.Console.CmdArgs
import qualified TmpIoJson as J

data Arguments = Arguments
    { graph :: String
    , input :: String
    } deriving (Show, Data, Typeable)

arguments = Arguments
    { graph = "no graph"
    , input = "no file"
    }

main :: IO String
main = do
    -- parse input
    inputArgs <- cmdArgs arguments
    let appGraph = parse . words . graph $ inputArgs
    --putStrLn $ "app graph:\n" ++ show appGraph ++ "\n"

    -- read io json format and parse it
    let ioJsonFiles = map (cmdPath2IoJsonPath . snd) [x | x <- appGraph, fst x /= END]
    ioJsons <- mapM J.readIoJson ioJsonFiles
    let ioJsons' = J.addEndIoJson ioJsons
    --putStrLn $ "io json formats:\n" ++ show ioJsons' ++ "\n"

    -- make json key adapter
    formatters <- mkFormatters ioJsons'
    --putStrLn $ "formatters:\n" ++ show formatters ++ "\n"

    -- build app and run it
    let app = mkApp appGraph ioJsons' formatters
    run app $ input inputArgs

cmdPath2IoJsonPath :: String -> String
cmdPath2IoJsonPath cmdPath = intercalate "/" (splitted ++ ["io.json"])
    where splitted = init $ splitOn "/" cmdPath

mkFormatters :: [J.TmpIoJson] -> IO [Formatter]
mkFormatters [] = return []
mkFormatters ioJsons = mapM (uncurry mkFormatter) ioForms
    where
        inHead = J.input $ head ioJsons
        outForms = init $ map J.output ioJsons
        inForms = tail $ map J.input ioJsons
        ioForms = (inHead, inHead) : zip outForms inForms

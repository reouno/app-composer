{-# LANGUAGE InstanceSigs #-}

module Node where

import BindType (Bind)
import Formatter (Formatter, InputFormat, OutputFormat)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

data Module = Module
    { bind :: Bind
    , modPath :: String
    , inType :: InputFormat
    , outType :: OutputFormat
    , formatter :: Formatter
    } deriving (Show, Eq)

class Node a where
    run :: a -> String -> IO String

instance Node Module where
    run module_ input = runModule (modPath module_) input'
        where
            input' = format (formatter module_) input

runModule :: String -> String -> IO String
runModule modPath input = do
    (status, stdout, stderr) <- readProcessWithExitCode modPath [input] ""
    case status of
        ExitSuccess -> return stdout
        ExitFailure n -> error ("process exit with status code " ++ show n ++ "\n" ++ "error message:\n" ++ stderr)

format :: Formatter -> String -> String
format rule input = input

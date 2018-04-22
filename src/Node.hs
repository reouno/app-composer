{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLabels #-}

module Node where

import Adapter (adapter, decodeJStr2Str)
import BindType (Bind)
import Formatter (Formatter, InputFormat, OutputFormat)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

-- appのノード
class Node a where
    run :: a -> String -> IO String

data Module = Module
    { bind :: Bind
    , modPath :: String
    , inType :: InputFormat
    , outType :: OutputFormat
    , formatter :: Formatter
    } deriving (Show, Eq)

instance Node Module where
    run module_ input = runModule (modPath module_) input'
        where
            input' = adapter (formatter (module_ :: Module)) input

-- 今の所は、最後のモジュールの出力をそのままappの出力とするが、
-- 将来的に出力形式も指定できるようにするため、フィールドを作っておく
data End = End
    { inType :: InputFormat
    , outType :: OutputFormat
    , formatter :: Formatter
    } deriving (Show, Eq)

instance Node End where
    run endNode input = runEnd $ adapter (formatter (endNode :: End)) input

runModule :: String -> String -> IO String
runModule modPath input = do
    putStrLn $ "Run " ++ show modPath
    --putStrLn $ "input:  " ++ show input
    (status, stdout, stderr) <- readProcessWithExitCode modPath [input] ""
    --putStrLn $ "output:  " ++ show stdout ++ "\n"
    case status of
        ExitSuccess -> return stdout
        ExitFailure n -> error ("process exit with status code " ++ show n ++ "\n" ++ "error message:\n" ++ stderr)

runEnd :: String -> IO String
runEnd input = do
    putStrLn "End"
    -- decodeJStr2Strは可視性のために使っているだけで、必須ではない
    let output = decodeJStr2Str input
    putStrLn output
    return output

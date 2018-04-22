module GraphParser where

import BindType (Bind(..))

-- 入力をパースして、各モジュールの結合方法とモジュール実行パスのリストを返す
-- parse input and return list of tuple of bind type and execution path to module
parse :: [String] -> [(Bind, String)]
parse [] = error "graph syntax error. found no module."
parse (s0:strs) = (START, s0) : parse' strs
    where
        parse' :: [String] -> [(Bind, String)]
        parse' [] = [(END, "")]
        parse' [s0] = error ("graph syntax error. module not found after binder \"" ++ s0 ++ "\".")
        parse' strs@(s0:s1:strs')
            | s0 == ">>="        = (BIND, s1) : parse' strs'
            | s0 == ">>"         = (THRU, s1) : parse' strs'
            | otherwise          = error ("graph syntax error. invalid binder: \"" ++ s0 ++ "\". \">>=\" and \">>\" are allowed to use.")

main :: IO ()
main = do
    --let strs0 = [] :: [String]
    --putStrLn $ "input:  " ++ show strs0 ++ "\noutput: " ++ show (parse strs0) ++ "\n"

    let strs1 = ["mod1"]
    putStrLn $ "input:  " ++ show strs1 ++ "\noutput: " ++ show (parse strs1) ++ "\n"

    let strs2 = ["mod1", ">>=", "mod2", ">>=", "mod3", ">>", "mod4"]
    putStrLn $ "input:  " ++ show strs2 ++ "\noutput: " ++ show (parse strs2) ++ "\n"

    -- raise runtime error
    let strs3 = ["mod1", ">>=", "mod2", ">>=", "mod3", ">>"]
    putStrLn $ "input:  " ++ show strs3 ++ "\noutput: " ++ show (parse strs3) ++ "\n"

    -- raise runtime error
    let strs4 = ["mod1", ">>==", "mod2"]
    putStrLn $ "input:  " ++ show strs4 ++ "\noutput: " ++ show (parse strs4) ++ "\n"

{-# LANGUAGE ExistentialQuantification #-}

module App where

import BindType (Bind)
import Formatter (Formatter)
import Node (Module(..), End(..), Node)
import TmpIoJson
import qualified Node as N

data Node' = forall a. (Node a, Show a) => Node' a

instance Show Node' where
    show (Node' n) = show n

type App = [Node']

runNode :: Node' -> String -> IO String
runNode (Node' n) = N.run n

run :: App -> String -> IO String
run [] _ = return ""
run [m] input = runNode m input
run (m:ms) input = run ms =<< runNode m input

mkApp :: [(Bind, String)] -> [TmpIoJson] -> [Formatter] -> App
mkApp [] [] [] = []
mkApp graphs ioJsons formatters = modules ++ [endNode]
    where
        --modules = map (Node' . uncurry3 mkModule) $ zip3 graphs ioJsons formatters
        modules =
            map (Node' . uncurry3 mkModule) $ zip3 (init graphs) (init ioJsons) (init formatters)
        endNode =
            Node' $ End (input (last ioJsons)) (output (last ioJsons)) (last formatters)

mkModule :: (Bind, String) -> TmpIoJson -> Formatter -> Module
mkModule (bind, modPath) ioJson =
    Module bind modPath (input ioJson) (output ioJson)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

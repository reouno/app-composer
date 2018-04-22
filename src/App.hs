module App where

import Adapter (adapter)
import BindType (Bind)
import Formatter (Formatter)
import Node (Module(..))
import TmpIoJson
import qualified Node as N

type App = [Module]

run :: App -> String -> IO String
run [] _ = return ""
run [m] input = do
    putStrLn ("Run " ++ show (N.modPath m))
    --putStrLn ("input:  " ++ show input ++ "\n")
    N.run m (adapter (N.formatter m) input)
run (m:ms) input = do
    putStrLn $ "Run " ++ show (N.modPath m)
    --putStrLn $ "input:  " ++ show input ++ "\n"
    input' <- N.run m $ adapter (N.formatter m) input
    run ms input'

mkApp :: [(Bind, String)] -> [TmpIoJson] -> [Formatter] -> App
mkApp [] [] [] = []
mkApp graphs ioJsons formatters =
    map (uncurry3 mkModule) $ zip3 graphs ioJsons formatters

mkModule :: (Bind, String) -> TmpIoJson -> Formatter -> Module
mkModule (bind, modPath) ioJson formatter =
    Module bind modPath (input ioJson) (output ioJson) formatter

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

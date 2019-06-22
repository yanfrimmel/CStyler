module Main where

import           System.Environment
import           Parser

invalidNumberOfArgumentsErrorMessage :: String
invalidNumberOfArgumentsErrorMessage =
    "Invalid number of arguments! arguments format: <["
        ++ parseToCArgumentName
        ++ "]/["
        ++ parseToOOArgumentName
        ++ "]> <path>"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [style, path] -> start style path
        _ -> putStrLn invalidNumberOfArgumentsErrorMessage

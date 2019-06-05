module Main where

import           System.Environment
import           Parser

invalidNumberOfArgumentsErrorMessage :: String
invalidNumberOfArgumentsErrorMessage =
    "Invalid number of arguments! arguments format: <["
        ++ parseToCArgumentName
        ++ "]/["
        ++ parseToOOArgumentName
        ++ "]> <["
        ++ isRecursiveArgumentName
        ++ "]> <path>"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [style, recursive, path] -> start style recursive path
        [style, path] -> start style "" path
        _ -> putStrLn invalidNumberOfArgumentsErrorMessage

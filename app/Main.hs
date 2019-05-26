module Main where

import           System.Environment
import           Parser

invalidNumberOfArgumentsErrorMessage :: String
invalidNumberOfArgumentsErrorMessage =
    "Invalid number of arguments! arguments format: <["
        ++ parseToCArgumentName
        ++ "]["
        ++ parseToOOArgumentName
        ++ "]> <file-path>"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [style, filePath] -> parse style filePath
        _                 -> putStrLn invalidNumberOfArgumentsErrorMessage

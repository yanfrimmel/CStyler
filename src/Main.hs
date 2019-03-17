module Main where

import System.Environment 
import Parser

main :: IO()
main = do
    args <- getArgs
    case args of
        [style, filePath] ->   parse style filePath
        _                 ->   putStrLn "Invalid number of arguments! arguments format: <style-type{toc}> file-path"
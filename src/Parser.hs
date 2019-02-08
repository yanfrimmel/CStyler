module Parser (parse) where

import System.IO  
import System.Directory

parseString :: String -> String
parseString input = parseWords $ words input

parse :: String -> String -> IO() 
parse style filePath = do
    if style == "toc" 
    then
        do
            handle <- openFile filePath ReadMode  
            (tempName, tempHandle) <- openTempFile "." "temp"  
            contents <- hGetContents handle  
            hPutStr tempHandle $ parseString contents  
            hClose handle  
            hClose tempHandle  
            removeFile filePath  
            renameFile tempName filePath  
    else
        do
            putStrLn "Invalid arguments! arguments format: <style-type{toc}> file-path"

parseWords :: [String] -> String
parseWords input = unwords input

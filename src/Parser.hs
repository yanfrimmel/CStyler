module Parser (parse) where

import System.IO  
import System.Directory
import Data.Char
import Data.List


parseToC :: String :: String
parseLineToC :: String -> String
parseLineToC str 
    | isStringCanBeChangedToC str  = concatMap (\y -> 
        if isUpper y 
        then '_' : toLower y : [] 
        else [y]) str
    | otherwise = str     

isStringCanBeChangedToC :: String -> Bool                                
isStringCanBeChangedToC str = (head str) == '#'

parse :: String -> String -> IO() 
parse style filePath = do
    if style == "toc" 
    then
        do
            handle <- openFile filePath ReadMode  
            (tempName, tempHandle) <- openTempFile "." "temp"  
            contents <- hGetContents handle  
            hPutStr tempHandle $ parseLineToC contents  
            hClose handle  
            hClose tempHandle  
            removeFile filePath  
            renameFile tempName filePath  
    else
        do
            putStrLn "Invalid arguments! arguments format: <style-type{toc}> file-path"
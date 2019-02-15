module Parser (parse) where

import System.IO  
import System.Directory
import Data.Char

parseToC :: String -> String
parseToC str =  unlines $ map (parseLineToC) strLines
    where strLines = lines str   

parseLineToC :: String -> String
parseLineToC str 
    | isLineCanBeChangedToC str  = 
        if second == '#' then str
        else if isAlpha curHead && isLower curHead && isAlpha second && isUpper second then
            curHead : '_' : toLower second : (parseLineToC $ tail $ tail $ str)
        else curHead : (parseLineToC $ tail str) 
    | otherwise = str    
    where curHead = head str
          second = 
            if length str > 1 then 
                head $ tail $ str
            else '#' -- just mark with dummy char to know that this is an empty list

isLineCanBeChangedToC :: String -> Bool                                
isLineCanBeChangedToC str = 
    if str == [] then 
        False 
    else (head str) /= '#'

parse :: String -> String -> IO() 
parse style filePath = do
    if style == "toc" 
    then
        do
            handle <- openFile filePath ReadMode  
            (tempName, tempHandle) <- openTempFile "." "temp"  
            contents <- hGetContents handle  
            hPutStr tempHandle $ parseToC contents  
            hClose handle  
            hClose tempHandle  
            removeFile filePath  
            renameFile tempName filePath  
    else
        do
            putStrLn "Invalid arguments! arguments format: <style-type{toc}> file-path"
module Parser (parse) where

import System.IO  
import System.Directory
import Data.Char

includeSymbol :: Char
includeSymbol = '#'

underScoreSymbol :: Char
underScoreSymbol = '_'

parseToCStyle :: String
parseToCStyle = "toc"

parseToC :: String -> String
parseToC str =  unlines $ map (parseLineToC) strLines
    where strLines = lines str   

isTwoLettersCamelCase :: Char -> Char -> Bool
isTwoLettersCamelCase firstLetter secondLetter = 
    isLower firstLetter && isUpper secondLetter

parseLineToC :: String -> String
parseLineToC str 
    | isLineCanBeChangedToC str  = 
        if secondLetter == includeSymbol then str
        else if (isTwoLettersCamelCase firstLetter secondLetter) then
            firstLetter : underScoreSymbol : toLower secondLetter : (parseLineToC $ tail $ tail $ str)
        else firstLetter : (parseLineToC $ tail str) 
    | otherwise = str    
    where firstLetter = head str
          secondLetter = 
            if length str > 1 then 
                head $ tail $ str
            else includeSymbol -- just mark with dummy char to know that this is an empty list

isLineCanBeChangedToC :: String -> Bool                                
isLineCanBeChangedToC str = 
    if str == [] then 
        False 
    else (head str) /= includeSymbol

parse :: String -> String -> IO() 
parse style filePath = do
    if style == parseToCStyle 
    then
        do
            handle <- openFile filePath ReadMode  
            (tempName, tempHandle) <- (openTempFile "." "temp")  
            contents <- hGetContents handle  
            hPutStr tempHandle $ parseToC contents  
            hClose handle  
            hClose tempHandle  
            removeFile filePath  
            renameFile tempName filePath  
    else
        do
            putStrLn "Invalid arguments! arguments format: <style-type{toc}> file-path"
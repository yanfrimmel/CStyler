module Parser
    ( parse
    , parseToC
    , parseFromC
    , parseToCArgumentName
    , parseFromCArgumentName
    )
where

import           System.IO
import           System.Directory
import           Data.Char

includeSymbol :: Char
includeSymbol = '#'

underScoreSymbol :: Char
underScoreSymbol = '_'

parseToCArgumentName :: String
parseToCArgumentName = "toc"

parseFromCArgumentName :: String
parseFromCArgumentName = "fromc"

invalidArgumentsErrorMessage :: String
invalidArgumentsErrorMessage =
    "Invalid arguments! arguments format: <["
        ++ parseToCArgumentName
        ++ "]["
        ++ parseFromCArgumentName
        ++ "]> <file-path>"

parseToC :: String -> String
parseToC str = unlines $ map parseLineToC strLines where strLines = lines str
-- TODO: implement - currently it has a duplicate body of parseToC
parseFromC :: String -> String
parseFromC str = unlines $ map parseLineFromC strLines
    where strLines = lines str


parseLineFromC :: String -> String
parseLineFromC str
    | isLineCanBeChanged str = if secondLetter == includeSymbol || thirdLetter == includeSymbol
        then str
        else if isUnderScoreBetweenTwoLowerLetters firstLetter secondLetter thirdLetter
            then
                firstLetter
                : toUpper thirdLetter
                : parseLineFromC (tail $ tail $ tail str)
            else firstLetter : parseLineFromC (tail str)
    | otherwise = str
    where
    firstLetter  = head str
    secondLetter = if length str > 1 then head $ tail str else includeSymbol -- just mark with dummy char to know that this is an empty list
    thirdLetter  = if length str > 2 then head $ tail $ tail str else includeSymbol -- just mark with dummy char to know that this is an empty list

        
parseLineToC :: String -> String
parseLineToC str
    | isLineCanBeChanged str = if secondLetter == includeSymbol
     then str
     else if isTwoLettersCamelCase firstLetter secondLetter
         then
             firstLetter
             : underScoreSymbol
             : toLower secondLetter
             : parseLineToC (tail $ tail str)
         else firstLetter : parseLineToC (tail str)
    | otherwise = str
  where
    firstLetter  = head str
    secondLetter = if length str > 1 then head $ tail str else includeSymbol -- just mark with dummy char to know that this is an empty list

isLineCanBeChanged :: String -> Bool
isLineCanBeChanged str = not (null str) && head str /= includeSymbol

parse :: String -> String -> IO ()
parse style filePath = do
    handle                     <- openFile filePath ReadMode
    (tempFilePath, tempHandle) <- openTempFile "." "temp"
    contents                   <- hGetContents handle
    if style == parseToCArgumentName
        then do
            hPutStr tempHandle $ parseToC contents
            onFileParseSuccess handle tempHandle filePath tempFilePath
        else if style == parseFromCArgumentName
            then do
                hPutStr tempHandle $ parseFromC contents
                onFileParseSuccess handle tempHandle filePath tempFilePath
            else onFileParseFailure handle tempHandle tempFilePath


onFileParseSuccess :: Handle -> Handle -> FilePath -> FilePath -> IO ()
onFileParseSuccess handle tempHandle filePath tempFilePath = do
    hClose handle
    hClose tempHandle
    removeFile filePath
    renameFile tempFilePath filePath

onFileParseFailure :: Handle -> Handle -> FilePath -> IO ()
onFileParseFailure handle tempHandle tempFilePath = do
    hClose handle
    hClose tempHandle
    removeFile tempFilePath
    putStrLn invalidArgumentsErrorMessage

isTwoLettersCamelCase :: Char -> Char -> Bool
isTwoLettersCamelCase firstLetter secondLetter =
    isLower firstLetter && isUpper secondLetter

isUnderScoreBetweenTwoLowerLetters :: Char -> Char -> Char -> Bool
isUnderScoreBetweenTwoLowerLetters firstLetter secondLetter thirdLetter =
    isLower firstLetter && underScoreSymbol == secondLetter && isLower thirdLetter    

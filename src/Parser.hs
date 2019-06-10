module Parser
    ( start
    , parse
    , parseToC
    , parseToOO
    , parseToCArgumentName
    , parseToOOArgumentName
    , isRecursiveArgumentName
    )
where

import           System.IO
import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           Data.Char
import           Control.Monad
import           Control.Concurrent.Async

includeSymbol :: Char
includeSymbol = '#'

underScoreSymbol :: Char
underScoreSymbol = '_'

parseToCArgumentName :: String
parseToCArgumentName = "-c"

parseToOOArgumentName :: String
parseToOOArgumentName = "-o"

isRecursiveArgumentName :: String
isRecursiveArgumentName = "-r"

invalidArgumentsErrorMessage :: String
invalidArgumentsErrorMessage =
    "Invalid arguments! arguments format: <["
        ++ parseToCArgumentName
        ++ "]|["
        ++ parseToOOArgumentName
        ++ "]> <file-path>"

parseToC :: String -> String
parseToC str = unlines $ map parseLineToC strLines where strLines = lines str

parseToOO :: String -> String
parseToOO str = unlines $ map parseLineToOO strLines
    where strLines = lines str


parseLineToOORememberLastLetter :: String -> Char -> String
parseLineToOORememberLastLetter str lastLetter
    | isLineCanBeChanged str
    = if isToProcessScanedLettersToOO lastLetter secondLetter thirdLetter
        then
            toUpper thirdLetter
                : parseLineToOORememberLastLetter (tail $ tail str) thirdLetter
        else parseLineToOO str
    | otherwise
    = str
  where
    secondLetter = head str
    thirdLetter  = getCharFromParsingStringOrGetEscapeChar str 1

parseLineToOO :: String -> String
parseLineToOO str
    | isLineCanBeChanged str
    = if isToProcessScanedLettersToOO firstLetter secondLetter thirdLetter
        then
            firstLetter
            : toUpper thirdLetter
            : parseLineToOORememberLastLetter (tail $ tail $ tail str)
                                              thirdLetter
        else firstLetter : parseLineToOO (tail str)
    | otherwise
    = str
  where
    firstLetter  = head str
    secondLetter = getCharFromParsingStringOrGetEscapeChar str 1
    thirdLetter  = getCharFromParsingStringOrGetEscapeChar str 2

parseLineToC :: String -> String
parseLineToC str
    | isLineCanBeChanged str
    = if isToProcessScanedLettersToC firstLetter secondLetter
        then
            firstLetter : underScoreSymbol : toLower secondLetter : parseLineToC
                (tail $ tail str)
        else firstLetter : parseLineToC (tail str)
    | otherwise
    = str
  where
    firstLetter  = head str
    secondLetter = getCharFromParsingStringOrGetEscapeChar str 1

getCharFromParsingStringOrGetEscapeChar :: String -> Int -> Char
getCharFromParsingStringOrGetEscapeChar str lessThanMinimumSize =
    if length str > lessThanMinimumSize
        then head $ iterate tail str !! lessThanMinimumSize
        else includeSymbol

isLineCanBeChanged :: String -> Bool
isLineCanBeChanged str = not (null str) && head str /= includeSymbol

isToProcessScanedLettersToC :: Char -> Char -> Bool
isToProcessScanedLettersToC firstLetter secondLetter =
    secondLetter
        /= includeSymbol
        && isTwoLettersCamelCase firstLetter secondLetter

isToProcessScanedLettersToOO :: Char -> Char -> Char -> Bool
isToProcessScanedLettersToOO firstLetter secondLetter thirdLetter =
    secondLetter
        /= includeSymbol
        && thirdLetter
        /= includeSymbol
        && isTreeLetterShouldChangeToOOStyle firstLetter
                                             secondLetter
                                             thirdLetter

start :: String -> String -> String -> IO ()
start style isRecursive filePath
    | isRecursive == isRecursiveArgumentName = do
        filePaths <- recursivelyGetFilePaths filePath
        mapConcurrently_ (\file -> parse style file) filePaths
    | otherwise = parse style filePath


recursivelyGetFilePaths :: FilePath -> IO [FilePath]
recursivelyGetFilePaths top = do
    directoryContents <- listDirectory top
    paths             <- forM directoryContents $ \subDir -> do
        let path = top </> subDir
        fileStatus <- getFileStatus path
        if isDirectory fileStatus
            then recursivelyGetFilePaths path
            else return [path]
    return (concat paths)


parse :: String -> String -> IO ()
parse style filePath = do
    print $ "processing: " ++ filePath ++ " ..."
    handle                     <- openFile filePath ReadMode
    (tempFilePath, tempHandle) <- openTempFile "." "temp"
    contents                   <- hGetContents handle
    if style == parseToCArgumentName
        then do
            hPutStr tempHandle $ parseToC contents
            onFileParseSuccess handle tempHandle filePath tempFilePath
        else if style == parseToOOArgumentName
            then do
                hPutStr tempHandle $ parseToOO contents
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

isTreeLetterShouldChangeToOOStyle :: Char -> Char -> Char -> Bool
isTreeLetterShouldChangeToOOStyle firstLetter secondLetter thirdLetter =
    isLower firstLetter
        && underScoreSymbol
        == secondLetter
        && isLower thirdLetter

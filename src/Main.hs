module Main where

import System.Environment
import System.Directory
import Data.String.Utils
import Debug.Trace
import Prelude
import Data.List.Split

isEndWithOneOf :: FilePath -> [String] -> Bool
isEndWithOneOf _ [] = False
isEndWithOneOf path (head:tail) = if head `endswith` path then True else isEndWithOneOf path tail

filterP :: [FilePath] -> [String] -> [String]
filterP [] _ = []
filterP (head:tail) suffix = (if head `isEndWithOneOf` suffix then [head] else []) ++ (filterP tail suffix)

getAllFileInDir::[FilePath] -> IO [FilePath]
getAllFileInDir [] = do return []
getAllFileInDir (head:tail) = do
    isDir <- doesDirectoryExist head
    funResult <-
     (if isDir then do
      result <- (listDirectory head >>= (\ subItem -> getAllFileInDir (tail ++ (map (\s -> head ++ "/" ++ s) subItem))))
      return result
     else do
      result <- getAllFileInDir tail
      return ([head] ++ result))
    return funResult

getCountNotEmptyLine :: [String] -> Int
getCountNotEmptyLine [] = 0
getCountNotEmptyLine (head:tail) = (if (length head) > 0 then 1 else 0) + (getCountNotEmptyLine tail)

getFilesLine :: [String] -> IO Int
getFilesLine [] = do
    return 0
getFilesLine (head:tail) = do
  contents <- readFile head
  count <- getFilesLine tail
  return (count + (getCountNotEmptyLine (splitOn "\n" contents)))

main :: IO ()
main = do
  (ditPath:extension) <- getArgs
  myFiles <- getAllFileInDir [ditPath]
  lineCount <- getFilesLine (filterP myFiles extension)
  putStrLn$show lineCount


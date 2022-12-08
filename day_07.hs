module Day2 where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List.Split (splitOn)
import Data.List (sort, intersect, intercalate)
import Data.Map as M
import Debug.Trace

tripNFirst :: Int -> String -> String
tripNFirst 0 s = s
tripNFirst n (s:xs) = tripNFirst (n-1) xs

append :: [String] -> Int -> Map [String] Int -> Map [String] Int
append [] val files = files
append path val files = do
    let value = M.lookup path files
    case value of
     Just existingVal -> append (init path) val (M.insert path (val+existingVal) files)
     Nothing -> append (init path) val (M.insert path val files)


getDirs :: [String] -> [String] -> Map [String] Int -> Map [String] Int
getDirs (word:ws) path files
    | word == "$ cd .." = getDirs ws (init path) files
    | word == "$ ls" || word !! 0 == 'd' = getDirs ws path files
    | word !! 2 == 'c' = getDirs ws (path ++ [tripNFirst 5 word]) files
    | otherwise = do
        let items = splitOn " " word
        let size = (read (items !! 0) :: Int)
        getDirs ws path (append path size files)
getDirs [] path files = files

main :: IO()
main = do 
    handle <- openFile "day_07.txt" ReadMode 
    content <- hGetContents handle
    let raw_data = splitOn "\n" content
    let dirs = getDirs raw_data [] M.empty
    let filteredDirs = filterWithKey (\name size -> size < 100000) dirs
    print(sum $ M.elems filteredDirs)
    let freeSpace = 70000000 - dirs ! ["/"]
    let requiredSpace = 30000000 - freeSpace
    let validDirs = filterWithKey (\name size -> size > requiredSpace) dirs
    print(minimum $ M.elems validDirs)

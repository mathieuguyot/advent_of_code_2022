module Day2 where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List.Split (splitOn)
import Data.List (sort, intersect)
import Data.Char

split :: [a] -> ([a], [a])
split myList = splitAt (((length myList) + 1) `div` 2) myList

computeScore :: Char -> Int
computeScore x = if isUpper x then (fromEnum x) - 38 else (fromEnum x) - 96

findItemType :: [[Char]] -> [Char] -> [Char]
findItemType (x:y:z:xs) items = items ++ [(head $ intersect x $ intersect y z)] ++ (findItemType xs items)
findItemType [] items = items

main :: IO()
main = do 
    handle <- openFile "day_03.txt" ReadMode 
    content <- hGetContents handle
    let raw_data = splitOn "\n" content
    let rules = map split raw_data
    print(sum $ map (\(x,y) -> computeScore $ head $ intersect x y) rules)
    print(sum $ map computeScore $ findItemType raw_data [])
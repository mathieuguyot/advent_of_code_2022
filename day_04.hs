module Day2 where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List.Split (splitOn)
import Data.List (sort, intersect)
import Data.Char

filterMapP1 :: [Int] -> Bool
filterMapP1 (a1:a2:b1:b2:xs) = if (a1 <= b1 && a2 >= b2) || (b1 <= a1 && b2 >= a2) then True else False

filterMapP2 :: [Int] -> Bool
filterMapP2 (a1:a2:b1:b2:xs) = if (a1 >= b1 && a1 <= b2) || (a2 >= b1 && a2 <= b2) || (b1 >= a1 && b2 <= a2) || (b2 >= a1 && b2 <= a2) then True else False

main :: IO()
main = do 
    handle <- openFile "day_04.txt" ReadMode 
    content <- hGetContents handle
    let raw_data = splitOn "\n" content
    let p_data_1 = map (splitOn ",") raw_data
    let p_data_2 = (map.map) (splitOn "-") p_data_1
    let p_data_3 = map (concat) p_data_2
    let pairs = (map.map) (\s -> read s :: Int) p_data_3
    print(length $ filter filterMapP1 pairs)
    print(length $ filter filterMapP2 pairs)

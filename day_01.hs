module Day1 where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List.Split
import Data.List (sort)

main :: IO()
main = do 
    handle <- openFile "day_01.txt" ReadMode 
    content <- hGetContents handle
    let raw_data = (map . map) (\s -> read s :: Int) $ map (splitOn "\n") $ splitOn "\n\n" content
    let sums = sort $ map sum raw_data
    print(last sums)
    print(sum $ drop (length sums - 3) sums)
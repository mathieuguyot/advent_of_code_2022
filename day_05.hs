module Day5 where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List.Split (splitOn)
import Data.List (splitAt)
import Debug.Trace

replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

drop' :: Int -> [a] -> [a]
drop' n xs = [ v | (_,v)  <- filter (\(x,_) -> x > n) $ zip [1..] xs ]

move :: Int -> Int -> Int -> [[Char]] -> [[Char]]
move 0 from to game = game
move count from to game = do
    let value = head $ game !! (from - 1)
    let newFrom = tail $ game !! (from - 1)
    let newTo = [value] ++ (game !! (to - 1))
    let newG = replaceAtIndex (from - 1) newFrom game
    let newG' = replaceAtIndex (to - 1) newTo newG
    move (count - 1) from to newG'

move' :: Int -> Int -> Int -> [[Char]] -> [[Char]]
move' count from to game = do
    let value = take count $ game !! (from - 1)
    let newFrom = drop' count $ game !! (from - 1)
    let newTo = value ++ (game !! (to - 1))
    let newG = replaceAtIndex (from - 1) newFrom game
    let newG' = replaceAtIndex (to - 1) newTo newG
    newG'

processData :: String -> (Int, Int, Int)
processData d = do
    let w = splitOn " " d
    (read (w !! 1) :: Int, read (w !! 3) :: Int, read (w !! 5) :: Int)

apply :: [(Int,Int,Int)] -> [[Char]] -> [[Char]]
apply [] game = game
apply ((a,b,c):xs) game = apply xs (move a b c game)

apply' :: [(Int,Int,Int)] -> [[Char]] -> [[Char]]
apply' [] game = game
apply' ((a,b,c):xs) game = apply' xs (move' a b c game)

main :: IO()
main = do 
    handle <- openFile "day_05.txt" ReadMode 
    content <- hGetContents handle
    let raw_data = map processData $ splitOn "\n" content
    let game = [['N','Z'], ['D','C','M'], ['P']]
    let real_game = [['P', 'L', 'M', 'N', 'W', 'V', 'B', 'H'], ['H', 'Q', 'M'], ['L', 'M', 'Q', 'F', 'G', 'B', 'D', 'N'], ['G', 'W', 'M', 'Q', 'F', 'T', 'Z'], ['P', 'H', 'T', 'Z'], ['T', 'G', 'H', 'D', 'J', 'M', 'B', 'C'], ['R', 'V', 'F', 'B', 'N', 'M'], ['S', 'G', 'R', 'M', 'H', 'L', 'P'], ['N', 'C', 'B', 'D', 'P']]
    print(raw_data)
    print(apply raw_data real_game)
    print(apply' raw_data real_game)

module Day2 where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List.Split
import Data.List (sort)

computeRoundScore :: (Char, Char) -> Int
computeRoundScore ('A', x) = if x == 'X' then 3 else if x == 'Y' then 6 else 0
computeRoundScore ('B', x) = if x == 'Y' then 3 else if x == 'Z' then 6 else 0
computeRoundScore ('C', x) = if x == 'Z' then 3 else if x == 'X' then 6 else 0

computeShapeScore :: (Char, Char) -> Int
computeShapeScore (_, 'Y') = 2
computeShapeScore (_, 'X') = 1
computeShapeScore (_, 'Z') = 3

computeScore :: (Char, Char) -> Int
computeScore round = computeRoundScore round + computeShapeScore round

alterRound :: (Char, Char) -> (Char, Char)
alterRound ('A', 'Y') = ('A', 'X')
alterRound ('B', 'Y') = ('B', 'Y')
alterRound ('C', 'Y') = ('C', 'Z')
alterRound ('A', 'X') = ('A', 'Z')
alterRound ('B', 'X') = ('B', 'X')
alterRound ('C', 'X') = ('C', 'Y')
alterRound ('A', 'Z') = ('A', 'Y')
alterRound ('B', 'Z') = ('B', 'Z')
alterRound ('C', 'Z') = ('C', 'X')

main :: IO()
main = do 
    handle <- openFile "day_02.txt" ReadMode 
    content <- hGetContents handle
    let rounds = map (\s -> (head s, last s)) $ splitOn "\n" content
    print(sum $ map computeScore rounds)
    print(sum $ map computeScore $ map alterRound rounds)
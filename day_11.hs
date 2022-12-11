{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List.Split
import Data.List (sort)
import Debug.Trace

type Items = [Int]
data Operation = Old | Num Int | Add Operation Operation | Times Operation Operation deriving (Show, Eq)
type Monkey = (Items, Operation, Int, Int, Int, Int)

parseOperation :: [String] -> Operation
parseOperation ("old":[]) = Old
parseOperation (x:"*":y:[]) = do
    let xv = if x == "old" then Old else Num (read x :: Int)
    let yv = if y == "old" then Old else Num (read y :: Int)
    Times xv yv
parseOperation (x:"+":y:[]) = do
    let xv = if x == "old" then Old else Num (read x :: Int)
    let yv = if y == "old" then Old else Num (read y :: Int)
    Add xv yv

parseMonkeyInput :: [String] -> Monkey
parseMonkeyInput xs = do
    let items = map (\x -> if last x == ',' then read (init x) :: Int else read x :: Int) $ drop 2 $ words (xs !! 1)
    let operation = parseOperation $ drop 3 $ words (xs !! 2)
    let divisible = head $ map (\x -> read x :: Int) $ drop 3 $ words (xs !! 3)
    let ifTrue = head $ map (\x -> read x :: Int) $ drop 5 $ words (xs !! 4)
    let ifFalse = head $ map (\x -> read x :: Int) $ drop 5 $ words (xs !! 5)
    (items, operation, divisible, ifTrue, ifFalse, 0)

applyOp :: Int -> Operation -> Int
applyOp old Old = old
applyOp old (Num x) = x
applyOp old (Add x y) = (applyOp old x) + (applyOp old y)
applyOp old (Times x y) = (applyOp old x) * (applyOp old y)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

processMonkey :: [Monkey] -> Int -> Int -> Bool -> [Monkey]
processMonkey monkeys id divider isDumb = do
    let (items, op, d, ifT, e, insp) = monkeys !! id
    case items of
        [] -> monkeys
        (i:is) -> 
            do
                let newValue = if isDumb then applyOp i op `div` 3 else applyOp i op `mod` divider
                let isDivisable = newValue `mod` d == 0
                let newMonkey = (is, op, d, ifT, e, (insp + 1))
                let targetMonkeyId = if isDivisable then ifT else e
                let (items', op', d', i', e', insp') = monkeys !! targetMonkeyId
                let newTarget = (items' ++ [newValue], op', d', i', e', insp')
                processMonkey (replaceNth id newMonkey $ replaceNth targetMonkeyId newTarget monkeys) id divider isDumb

processMonkeys :: Int -> [Monkey] -> Int -> Bool -> [Monkey]
processMonkeys id monkeys divider isDumb
    | id == length monkeys = monkeys
    | otherwise = processMonkeys (id+1) (processMonkey monkeys id divider isDumb) divider isDumb

cycleMonkeys :: Int -> [Monkey] -> Int -> Bool -> [Monkey]
cycleMonkeys 0 monkeys divider isDumb = monkeys
cycleMonkeys x monkeys divider isDumb = cycleMonkeys (x-1) (processMonkeys 0 monkeys divider isDumb) divider isDumb

main :: IO()
main = do
    handle <- openFile "day_11.txt" ReadMode
    content <- hGetContents handle
    let raw_data = map (splitOn "\n")  $ splitOn "\n\n" content
    let monkeys = map parseMonkeyInput raw_data
    let divider = product $ map (\(_, _, d, _, _, _) -> d) monkeys
    let dumbMonkeys = cycleMonkeys 20 monkeys divider True
    let chadMonkeys = cycleMonkeys 10000 monkeys divider False
    let dumbValues = sort $ map (\(_, _, _, _, _, i) -> i) dumbMonkeys
    let chadValues = sort $ map (\(_, _, _, _, _, i) -> i) chadMonkeys
    print(last dumbValues * (last $ init dumbValues))
    print(last chadValues * (last $ init chadValues))
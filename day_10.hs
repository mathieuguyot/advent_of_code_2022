module Day8 where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List (sort, intersect, intercalate, group)
import Debug.Trace

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

data Instruction = Noop Int | AddX Int Int deriving (Show, Eq)

execute :: Int -> Int -> [Instruction] -> [(Int, Int, Char)]
execute _ _ [] = []
execute cycle x (inst:insts) = do
    let newCycle = cycle+1
    let (m, r) = quotRem cycle 40
    let pixel = if r `elem` [x, x+1, x+2] then '#' else '.'
    let res = [(cycle, x, pixel)]
    case inst of 
        Noop _ -> res ++ (execute newCycle x insts)
        AddX v 2 -> res ++ (execute newCycle x ([AddX v 1]++insts))
        AddX v 1 -> res ++ (execute newCycle (x+v) insts)

parseInstruction :: String -> Instruction
parseInstruction "noop" = Noop 1
parseInstruction line = do
    let items = words line
    let num = read (items !! 1) :: Int
    AddX num 2

printCRT :: String -> [String]
printCRT "" = []
printCRT s = [(take 40 s)] ++ printCRT (drop 40 s)

main :: IO()
main = do
    handle <- openFile "day_10.txt" ReadMode
    content <- hGetContents handle
    let lines = wordsWhen (== '\n') content
    let instructions = map parseInstruction lines
    let executionRes = execute 1 1 instructions
    let cycles = [20, 60, 100, 140, 180, 220]
    print(sum $ map (\(x, y, _) -> x * y) $ filter (\(x, _, _) -> x `elem` cycles) executionRes)
    mapM_ print $ printCRT $ map (\(_,_,c)->c) executionRes
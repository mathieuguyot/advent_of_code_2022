module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List.Split
import Data.List (sort)
import Debug.Trace
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Position = (Int, Int)
type Value = Int
type Cell = (Position, Value)
type CellMap = Map.Map Position Value
type UnvisitedSet = Set.Set Position

getAdjacentPoints :: Cell -> CellMap -> [Position]
getAdjacentPoints ((x, y), val) nodes = do
    let isMember1 = Map.member (x + 1, y) nodes
    let isMember2 = Map.member (x - 1, y) nodes
    let isMember3 = Map.member (x, y + 1) nodes
    let isMember4 = Map.member (x, y - 1) nodes
    let member1 = if isMember1 && nodes Map.! (x + 1, y) <= (val+1) then [(x + 1, y)] else []
    let member2 = if isMember2 && nodes Map.! (x - 1, y) <= (val+1) then [(x - 1, y)] else []
    let member3 = if isMember3 && nodes Map.! (x, y + 1) <= (val+1) then [(x, y + 1)] else []
    let member4 = if isMember4 && nodes Map.! (x, y - 1) <= (val+1) then [(x, y - 1)] else []
    member1 ++ member2 ++ member3 ++ member4

insertInOrder :: Cell -> [Cell] -> [Cell]
insertInOrder cell [] = [cell]
insertInOrder cell (d:ds) = if snd d < snd cell then d : insertInOrder cell ds else cell : d : ds

first :: Position -> [Cell] -> Cell
first pos (a:as) = if pos == fst a then a else first pos as

checkAdjacent :: Int -> Cell -> [Cell] -> [Cell]
checkAdjacent n cur distances = do
    let newDist = n + 1
    let oldDist = snd $ first (fst cur) distances
    let newDistances = insertInOrder (fst cur, newDist) $ filter (\(p, v) -> p /= fst cur) distances
    if newDist < oldDist then newDistances else distances

dijkstra :: Cell -> CellMap -> [Cell] -> UnvisitedSet -> IO()
dijkstra cur nodes distances unvisited = do
    let unvisitedAdjNodes = getAdjacentPoints cur nodes
    let curDistance = snd $ head distances
    let d = tail distances ++ [head distances]
    let nD1 = if length unvisitedAdjNodes >= 1 then checkAdjacent curDistance (unvisitedAdjNodes !! 0, nodes Map.! (unvisitedAdjNodes !! 0)) d else d
    let nD2 = if length unvisitedAdjNodes >= 2 then checkAdjacent curDistance (unvisitedAdjNodes !! 1, nodes Map.! (unvisitedAdjNodes !! 1)) nD1 else nD1
    let nD3 = if length unvisitedAdjNodes >= 3 then checkAdjacent curDistance (unvisitedAdjNodes !! 2, nodes Map.! (unvisitedAdjNodes !! 2)) nD2 else nD2
    let nD4 = if length unvisitedAdjNodes >= 4 then checkAdjacent curDistance (unvisitedAdjNodes !! 3, nodes Map.! (unvisitedAdjNodes !! 3)) nD3 else nD3
    let newUnvisited = Set.delete (fst cur) unvisited
    let newCurrentVal = nodes Map.! (fst $ head nD4)
    if null newUnvisited then print nD4 else dijkstra (fst $ head nD4, newCurrentVal) nodes nD4 newUnvisited

charToNum :: Char -> Int
charToNum 'S' = 1
charToNum 'E' = 28
charToNum x = fromEnum x - 95
    
parseNodes :: [String] -> [Cell]
parseNodes map = parse' map 0
    where parse' :: [String] -> Int -> [Cell]
          parse' [] _         = []
          parse' (x:xs) index = [((index, y), charToNum $ x !! y) | y <- [0..(length x-1)]] ++ parse' xs (index+1)

initTentativeDistance :: Cell -> [Cell] -> [Cell]
initTentativeDistance (p, _) cells = map (\(p2, v) -> if v == 1 || v == 2 then (p2, 0) else (p2, 1000000000)) cells

main :: IO()
main = do
    handle <- openFile "day_12.txt" ReadMode
    content <- hGetContents handle
    let nodes = parseNodes $ (splitOn "\n") content
    let nodesMap = Map.fromList nodes
    let startNode = head $ filter (\(_, v) -> v == 1) nodes
    print(startNode)
    print(nodes)
    print(head $ filter (\(_, v) -> v == 28) nodes)
    dijkstra startNode nodesMap (initTentativeDistance startNode nodes) (Set.fromList $ map fst nodes)
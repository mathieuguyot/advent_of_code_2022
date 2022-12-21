{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List (sort, group, groupBy, sortBy, find, subsequences)
import Debug.Trace
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

type ValveName = String
type Minute = Int
type Pressure = Int
data Valve = Valve {
    name :: ValveName,
    isOpen :: Bool,
    flow :: Int,
    tunnels :: [ValveName],
    openedAtMinute :: Int
} deriving(Show, Read)

-- BEGIN HOLY DIJKTRA SECTION --
getAdjacentPoints :: ValveName -> [Valve] -> [ValveName]
getAdjacentPoints currentValveName valves = do
    let currentValve = find (\Valve{name} -> name == currentValveName) valves
    case currentValve of
        Nothing -> []
        Just (Valve _ _ _ tunnels _) -> tunnels

initTentativeDistance :: ValveName -> [Valve] -> [(ValveName, Int)]
initTentativeDistance currentValveName valves = map (\Valve{name} -> if name == currentValveName then (name, 0) else (name, 1000000000)) valves

insertInOrder :: (ValveName, Int) -> [(ValveName, Int)] -> [(ValveName, Int)]
insertInOrder v [] = [v]
insertInOrder c (d:ds) = if snd d < snd c then d : insertInOrder c ds else c : d : ds

first :: ValveName -> [(ValveName, Int)] -> (ValveName, Int)
first name (a:as) = if name == fst a then a else first name as

checkAdjacent :: Int -> ValveName -> [(ValveName, Int)] -> [(ValveName, Int)]
checkAdjacent curDist curName distances = do
    let newDist = curDist + 1
    let oldDist = snd $ first curName distances
    let newDistances = insertInOrder (curName, newDist) $ filter (\(p, v) -> p /= curName) distances
    if newDist < oldDist then newDistances else distances

type UnvisitedSet = Set.Set ValveName
dijkstra :: ValveName -> [Valve] -> [(ValveName, Int)] -> UnvisitedSet -> [(ValveName, Int)]
dijkstra name valves distances unvisited = do
    let unvisitedAdjNodes = getAdjacentPoints name valves
    let curDistance = snd $ head distances
    let d = tail distances ++ [head distances]
    let nD1 = if length unvisitedAdjNodes >= 1 then checkAdjacent curDistance (unvisitedAdjNodes !! 0) d else d
    let nD2 = if length unvisitedAdjNodes >= 2 then checkAdjacent curDistance (unvisitedAdjNodes !! 1) nD1 else nD1
    let nD3 = if length unvisitedAdjNodes >= 3 then checkAdjacent curDistance (unvisitedAdjNodes !! 2) nD2 else nD2
    let nD4 = if length unvisitedAdjNodes >= 4 then checkAdjacent curDistance (unvisitedAdjNodes !! 3) nD3 else nD3
    let newUnvisited = Set.delete name unvisited
    if null newUnvisited then nD4 else dijkstra (fst $ head nD4) valves nD4 newUnvisited

createDistances :: Int -> [Valve] -> [Valve] -> [(ValveName, ValveName, Int)]
createDistances maxMin [] values = []
createDistances maxMin (Valve{name}:vs) valves = do 
    let valvesNames = Set.fromList $ map (\Valve{name} -> name) valves
    let valuableValvesNames = Set.fromList $ map (\Valve{name} -> name) $ filter (\Valve{name, flow} -> flow > 0 || name == "AA") valves
    let curDijk = dijkstra name valves (initTentativeDistance name valves) valvesNames
    let pairs = map (\(to, val) -> (name, to, val)) curDijk
    let filteredPairs = filter (\(from, to, x) -> x > 0 && x < maxMin && from `elem` valuableValvesNames && to `elem` valuableValvesNames) pairs
    filteredPairs ++ createDistances maxMin vs valves
-- END HOLY DIJKTRA SECTION --

-- BEGIN PARSING SECTION --
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

parseValve :: String -> Valve
parseValve s = do
    let ws = words s
    let name = ws !! 1
    let isOpen = False
    let openedAtMinute = -1
    let flow = read (drop 5 (init (ws !! 4))) :: Int
    let tunnels = map (\(x:y:rest) -> [x,y]) (drop 9 ws)
    Valve{..}
-- END PARSING SECTION --

computePression :: Int -> [Valve] -> Int
computePression maxMin [] = 0
computePression maxMin (Valve _ _ flow _ openedAtMinute:rest) = value + computePression maxMin rest where
    value = if openedAtMinute > 0 && openedAtMinute < maxMin then (maxMin-openedAtMinute)*flow else 0

openValve :: String -> Int -> [Valve] -> [Valve]
openValve curName t valves = map (\Valve{..} -> Valve{name, isOpen=if curName == name then True else isOpen, flow, tunnels, openedAtMinute=if curName == name then t else openedAtMinute}) valves

areStillToOpen :: [Valve] -> Bool
areStillToOpen v = length (filter (\Valve{..} -> not isOpen && flow > 0) v) > 0

findMaxPressure :: Int -> [ValveName] -> (ValveName, Minute, [Valve]) -> [(ValveName, ValveName, Int)] -> Int
findMaxPressure maxMin allowedValvesNames (currentValveName, n, valves) valuableDists
    | n >= maxMin = computePression maxMin valves
    | otherwise = if not (areStillToOpen valves) then currentPressure else maximum([currentPressure] ++ childPressures) where
        currentPressure = computePression maxMin valves
        closedValves = map (\Valve{name} -> name) $ filter (\Valve{..} -> not isOpen && flow > 0 && name `elem` allowedValvesNames) valves
        currentValve = find (\Valve{name} -> name == currentValveName) valves
        potentialChildsDists = filter (\(from, to, val) -> from == currentValveName && to `elem` closedValves && to /= "AA") valuableDists
        childPressures = map (\(from, to, val) -> findMaxPressure maxMin allowedValvesNames (to, n + val + 1, (openValve to (n + val + 1) valves)) valuableDists) potentialChildsDists

maxMinP1 = 30
maxMinP2 = 26

combinations k ns = filter ((k==).length) $ subsequences ns

main :: IO()
main = do 
    handle <- openFile "day_16.txt" ReadMode 
    content <- hGetContents handle

    let lines = wordsWhen (=='\n') content
    let valves = map parseValve lines
    let valvesNames = Set.fromList $ map (\Valve{name} -> name) valves
    let valuableDistsP1 = createDistances maxMinP1 valves valves
    let allowedValvesP1 = map (\Valve{name} -> name) $ filter (\Valve{flow} -> flow > 0) valves
    --print(findMaxPressure maxMinP1 allowedValvesP1 ("AA", 0, valves) valuableDistsP1)

    let valuableDistsP2 = createDistances maxMinP2 valves valves
    let couplesP2 = map (\l -> (l,filter (\i -> not (i `elem` l)) allowedValvesP1)) $ combinations ((length allowedValvesP1 `div` 2)-2) allowedValvesP1
    print(length couplesP2)
    let maxPressures = map (\(l1,l2) -> (findMaxPressure maxMinP2 l1 ("AA", 0, valves) valuableDistsP2) + (findMaxPressure maxMinP2 l2 ("AA", 0, valves) valuableDistsP2)) couplesP2
    print(maxPressures)
    print(maximum maxPressures)
    -- 2649 & 2700 too low
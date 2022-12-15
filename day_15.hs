module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List (sort, group, groupBy, sortBy, find)
import Debug.Trace
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

data CellType = Sensor Int | Beacon | Reachable deriving (Show, Read, Eq, Ord)
type Position = (Int, Int)
type Cell = (Position, CellType)

-- BEGIN PARSING SECTION
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

parseLine :: String -> [Cell]
parseLine s = do
    let w = words s
    let sx = read ((init . tail . tail) (w !! 2)) :: Int
    let sy = read ((init . tail . tail) (w !! 3)) :: Int
    let bx = read ((init . tail . tail) (w !! 8)) :: Int
    let by = read ((tail . tail) (w !! 9)) :: Int
    let reach = abs (sx - bx) + abs (sy - by)
    [((sx, sy), Sensor reach), ((bx, by), Beacon)]
-- END PARSING SECTION

-- BEGIN INIT WORLD SECTION
magicNum = 2000000
maginFreqNum = 4000000

generatePositions :: Cell -> [Cell]
generatePositions ((x,y), Sensor n) = [((rx, ry), Reachable) | rx <- [x-n..x+n], ry <- [magicNum], (abs (x - rx) + abs (y - ry)) <= n && ry == magicNum]

frequency :: Position -> Int 
frequency (x,y) = x * 4000000 + y

manhattan :: Position -> Position -> Int
manhattan (x,y) (x2,y2) = abs (x - x2) + abs (y - y2)

doesPosOverlapSensors :: Position -> [Cell] -> Bool
doesPosOverlapSensors pos [] = False -- FOUND IT
doesPosOverlapSensors pos ((curSensorPos, Sensor maxDist):otherSensors) = do
    let curDist = manhattan pos curSensorPos
    if curDist <= maxDist then True else doesPosOverlapSensors pos otherSensors

-- https://www.codeproject.com/Questions/224182/Get-all-points-in-a-Line
--checkInSensorLine :: allSensors -> pos1 -> pos2
checkInSensorLine :: [Cell] -> Position -> Position -> Maybe Position
checkInSensorLine sensors (x1,y1) (x2,y2) = do
    let m = (y1-y2) `div` (x1-x2)
    let c = y1 - x1 * m
    let linesPos = [(x, m*x+c) | x <- [minimum([x1,x2])..maximum([x1,x2])], x > 0 && m*x+c > 0 && x <= maginFreqNum && m*x+c <= maginFreqNum && not (doesPosOverlapSensors (x, m*x+c) sensors)]
    if length linesPos > 0 then Just (head linesPos) else Nothing

checkForLostBeacon :: [Cell] -> Cell -> Maybe Position
checkForLostBeacon allSensors ((x,y), Sensor n)
    | pointsTR /= Nothing = pointsTR
    | pointsRB /= Nothing = pointsRB
    | pointsBL /= Nothing = pointsBL
    | pointsLT /= Nothing = pointsLT
    | otherwise = Nothing
    where
        top = (x+n+1, y)
        bottom = (x-n-1, y)
        left = (x,y-n-1)
        right = (x,y+n+1)
        pointsTR = checkInSensorLine allSensors top right
        pointsRB = checkInSensorLine allSensors right bottom
        pointsBL = checkInSensorLine allSensors bottom left 
        pointsLT = checkInSensorLine allSensors left top
-- END COMPUTATION SECTION

main :: IO()
main = do 
    handle <- openFile "day_15.txt" ReadMode 
    content <- hGetContents handle
    let lines = wordsWhen (=='\n') content
    let world = Map.fromList $ concatMap parseLine lines
    let sensors = Map.filter (\v -> v /= Beacon && v /= Reachable) world
    let reachable = concatMap (\v -> (generatePositions (v))) (Map.toList sensors)
    let newWorld = Map.union world (Map.fromList reachable)
    print(Map.size $ Map.filterWithKey (\(x, y) v -> y == magicNum && v == Reachable) newWorld)
    let (Just (Just (pos))) = find (/= Nothing) $ map (\v -> (checkForLostBeacon (Map.toList sensors) (v))) (Map.toList sensors)
    print(frequency pos)
    --print(getAllPoints (10,10) (5,0))
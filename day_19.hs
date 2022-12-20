{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode))
import Data.List (sort, group, groupBy, sortBy, find)
import Debug.Trace
import Data.Function (fix)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

parseBlueprint :: String -> Blueprint
parseBlueprint s = do
    let w = words s
    let oreRobotOreCost = read (w !! 6) :: Int
    let clayRobotOreCost = read (w !! 12) :: Int
    let obstidianRobotOreCost = read (w !! 18) :: Int
    let obstidianRobotClayCost = read (w !! 21) :: Int
    let geodeRobotOreCost = read (w !! 27) :: Int
    let geodeRobotObstidianCost = read (w !! 30) :: Int
    Blueprint{..}

data Blueprint = Blueprint {
    oreRobotOreCost :: Int,
    clayRobotOreCost :: Int,
    obstidianRobotOreCost :: Int,
    obstidianRobotClayCost :: Int,
    geodeRobotOreCost :: Int,
    geodeRobotObstidianCost :: Int
} deriving(Show, Read, Eq)

data Simulation = Simulation {
    oreRobotCount :: Int,
    clayRobotCount :: Int,
    obstidianRobotCount :: Int,
    geodeRobotCount :: Int,
    ore :: Int,
    clay :: Int,
    obstidian :: Int,
    geode :: Int,
    turn :: Int
} deriving(Show, Read, Eq)

initSimulation = Simulation 1 0 0 0 0 0 0 0 0

turnNeededToGet :: Int -> Int -> Int -> Int
turnNeededToGet costNeed actualRessource productionRate
    | costNeed <= actualRessource = 1
    | otherwise = 1 + turnNeededToGet costNeed (actualRessource + productionRate) productionRate

findMaxGeodes :: Int -> [Simulation] -> Blueprint -> Simulation -> (Int, [Simulation])
findMaxGeodes maxTurn prevs blueprint@Blueprint{..} simu@Simulation{..} = do
    let turnLeft = maxTurn - turn

    let turnsToBuildOreRobot = turnNeededToGet oreRobotOreCost ore oreRobotCount
    let canBuildOreRobot = turnLeft > turnsToBuildOreRobot && clayRobotCount <= 1 && (oreRobotCount <= maximum [oreRobotOreCost, clayRobotOreCost, obstidianRobotOreCost, geodeRobotOreCost])
    let buildOreRobot = findMaxGeodes maxTurn (prevs ++ [simu]) blueprint (Simulation (oreRobotCount + 1) clayRobotCount obstidianRobotCount geodeRobotCount (ore - oreRobotOreCost + oreRobotCount * turnsToBuildOreRobot) (clay + clayRobotCount * turnsToBuildOreRobot) (obstidian + obstidianRobotCount * turnsToBuildOreRobot) (geode + geodeRobotCount * turnsToBuildOreRobot) (turn + turnsToBuildOreRobot))

    let turnsToBuildClayRobot = turnNeededToGet clayRobotOreCost ore oreRobotCount
    let canBuildClayRobot = turnLeft > turnsToBuildClayRobot && clayRobotCount <= obstidianRobotClayCost
    let buildClayRobot = findMaxGeodes maxTurn (prevs ++ [simu]) blueprint (Simulation oreRobotCount (clayRobotCount + 1) obstidianRobotCount geodeRobotCount (ore - clayRobotOreCost + oreRobotCount * turnsToBuildClayRobot) (clay + clayRobotCount * turnsToBuildClayRobot) (obstidian + obstidianRobotCount * turnsToBuildClayRobot) (geode + geodeRobotCount * turnsToBuildClayRobot) (turn + turnsToBuildClayRobot))

    let turnsToBuildObstidianRobot = maximum [turnNeededToGet obstidianRobotOreCost ore oreRobotCount, turnNeededToGet obstidianRobotClayCost clay clayRobotCount]
    let canBuildObstidianRobot = clayRobotCount >= 1 && turnLeft > turnsToBuildObstidianRobot && obstidianRobotCount <= geodeRobotObstidianCost
    let buildObstidianRobot = findMaxGeodes maxTurn (prevs ++ [simu]) blueprint (Simulation oreRobotCount clayRobotCount (obstidianRobotCount + 1) geodeRobotCount (ore - obstidianRobotOreCost + oreRobotCount * turnsToBuildObstidianRobot) (clay - obstidianRobotClayCost + clayRobotCount * turnsToBuildObstidianRobot) (obstidian + obstidianRobotCount * turnsToBuildObstidianRobot) (geode + geodeRobotCount * turnsToBuildObstidianRobot) (turn + turnsToBuildObstidianRobot))

    let turnsToBuildGeodeRobot = maximum [turnNeededToGet geodeRobotOreCost ore oreRobotCount, turnNeededToGet geodeRobotObstidianCost obstidian obstidianRobotCount]
    let canBuildGeodeRobot = obstidianRobotCount >= 1 && turnLeft > turnsToBuildGeodeRobot
    let buildGeodeRobot = findMaxGeodes maxTurn (prevs ++ [simu]) blueprint (Simulation oreRobotCount clayRobotCount obstidianRobotCount (geodeRobotCount + 1) (ore - geodeRobotOreCost + oreRobotCount * turnsToBuildGeodeRobot) (clay + clayRobotCount * turnsToBuildGeodeRobot) (obstidian - geodeRobotObstidianCost + obstidianRobotCount * turnsToBuildGeodeRobot) (geode + geodeRobotCount * turnsToBuildGeodeRobot) (turn + turnsToBuildGeodeRobot))

    let canBuildNoRobot = not canBuildOreRobot && not canBuildClayRobot && not canBuildObstidianRobot && not canBuildGeodeRobot
    let buildRobots = foldr (\(i,p) (i',p') -> if i > i' then (i,p) else (i',p')) (0, []) [if canBuildOreRobot then buildOreRobot else (0, []), if canBuildClayRobot then buildClayRobot else (0, []), if canBuildObstidianRobot then buildObstidianRobot else (0, []), if canBuildGeodeRobot then buildGeodeRobot else (0, [])]
    let result = if canBuildNoRobot then (geode + geodeRobotCount * turnLeft, prevs ++ [simu]) else if canBuildGeodeRobot && turnsToBuildGeodeRobot == 1 then buildGeodeRobot else buildRobots
    result

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [1..]

maxRoundP1 = 24
maxRoundP2 = 32

main = do
    handle <- openFile "day_19.txt" ReadMode 
    content <- hGetContents handle
    let blueprints = map parseBlueprint $ wordsWhen (=='\n') content
    let simus = map (\b -> findMaxGeodes maxRoundP1 [] b initSimulation) blueprints
    let v = map fst simus
    print(sum $ mapInd (\a i -> a * i) v)
    print(product $ map fst $ map (\b -> findMaxGeodes maxRoundP2 [] b initSimulation) (take 3 blueprints))
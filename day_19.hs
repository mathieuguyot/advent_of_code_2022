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

findMaxGeodes :: Int -> [Simulation] -> Blueprint -> Simulation -> (Int, [Simulation])
findMaxGeodes maxTurn prevs blueprint@Blueprint{..} simu@Simulation{..} = do
    let turnLeft = maxTurn - turn

    let turnsToBuildOreRobot = if ore >= oreRobotOreCost then 0 else round ((fromIntegral (oreRobotOreCost - ore)) / (fromIntegral oreRobotCount))
    let canBuildOreRobot = turnLeft > turnsToBuildOreRobot && clayRobotCount < 1
    let buildOreRobot = findMaxGeodes maxTurn (prevs ++ [simu]) blueprint (Simulation (oreRobotCount + 1) clayRobotCount obstidianRobotCount geodeRobotCount (ore - oreRobotOreCost + oreRobotCount * (turnsToBuildOreRobot+1)) (clay + clayRobotCount * (turnsToBuildOreRobot+1)) (obstidian + obstidianRobotCount * (turnsToBuildOreRobot+1)) (geode + geodeRobotCount * (turnsToBuildOreRobot+1)) (turn + turnsToBuildOreRobot + 1))

    let turnsToBuildClayRobot = if ore >= clayRobotOreCost then 0 else round ((fromIntegral (clayRobotOreCost - ore)) / (fromIntegral oreRobotCount))
    let canBuildClayRobot = turnLeft > turnsToBuildClayRobot
    let buildClayRobot = findMaxGeodes maxTurn (prevs ++ [simu]) blueprint (Simulation oreRobotCount (clayRobotCount + 1) obstidianRobotCount geodeRobotCount (ore - clayRobotOreCost + oreRobotCount * (turnsToBuildClayRobot+1)) (clay + clayRobotCount * (turnsToBuildClayRobot+1)) (obstidian + obstidianRobotCount * (turnsToBuildClayRobot+1)) (geode + geodeRobotCount * (turnsToBuildClayRobot+1)) (turn + turnsToBuildClayRobot + 1))
    
    let turnsToBuildObstidianRobot = if ore >= obstidianRobotOreCost && clay >= obstidianRobotClayCost then 0 else maximum [round ((fromIntegral (obstidianRobotOreCost - ore)) / (fromIntegral oreRobotCount)), round ((fromIntegral (obstidianRobotClayCost - clay)) / (fromIntegral clayRobotCount))]
    let canBuildObstidianRobot = clayRobotCount >= 1 && turnLeft > turnsToBuildObstidianRobot
    let buildObstidianRobot = findMaxGeodes maxTurn (prevs ++ [simu]) blueprint (Simulation oreRobotCount clayRobotCount (obstidianRobotCount + 1) geodeRobotCount (ore - obstidianRobotOreCost + oreRobotCount * (turnsToBuildObstidianRobot+1)) (clay - obstidianRobotClayCost + clayRobotCount * (turnsToBuildObstidianRobot+1)) (obstidian + obstidianRobotCount * (turnsToBuildObstidianRobot+1)) (geode + geodeRobotCount * (turnsToBuildObstidianRobot+1)) (turn + turnsToBuildObstidianRobot + 1))
    
    let turnsToBuildGeodeRobot = if ore >= geodeRobotOreCost && obstidian >= geodeRobotObstidianCost then 0 else maximum [round ((fromIntegral (geodeRobotOreCost - ore)) / (fromIntegral oreRobotCount)), round ((fromIntegral (geodeRobotObstidianCost - obstidian)) / (fromIntegral obstidianRobotCount))]
    let canBuildGeodeRobot = obstidianRobotCount >= 1 && turnLeft > turnsToBuildGeodeRobot
    let buildGeodeRobot = findMaxGeodes maxTurn (prevs ++ [simu]) blueprint (Simulation oreRobotCount clayRobotCount obstidianRobotCount (geodeRobotCount+1) (ore - geodeRobotOreCost + oreRobotCount * (turnsToBuildGeodeRobot+1)) (clay + clayRobotCount * (turnsToBuildGeodeRobot+1)) (obstidian - geodeRobotObstidianCost + obstidianRobotCount * (turnsToBuildGeodeRobot+1)) (geode + geodeRobotCount * (turnsToBuildGeodeRobot+1)) (turn + turnsToBuildGeodeRobot + 1))

    let canBuildNoRobot = not canBuildOreRobot && not canBuildClayRobot && not canBuildObstidianRobot && not canBuildGeodeRobot
    let buildRobots = foldr (\(i,p) (i',p') -> if i > i' then (i,p) else (i',p')) (0, []) [if canBuildOreRobot then buildOreRobot else (0, []), if canBuildClayRobot then buildClayRobot else (0, []), if canBuildObstidianRobot then buildObstidianRobot else (0, []), if canBuildGeodeRobot then buildGeodeRobot else (0, [])]
    let result = if canBuildNoRobot then (geode + geodeRobotCount * turnLeft, prevs ++ [simu]) else buildRobots
    result

main = do
    print("DAY 19")
    let (v, prevs) = findMaxGeodes 23 [] (Blueprint 4 2 3 14 2 7) initSimulation
    print v
    mapM_ putStrLn (map show prevs)
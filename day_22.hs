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

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case Prelude.dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

rotatePlayer :: Facing -> Bool -> Facing
rotatePlayer UpF rotateClockwise = if rotateClockwise then RightF else LeftF
rotatePlayer RightF rotateClockwise = if rotateClockwise then DownF else UpF
rotatePlayer DownF rotateClockwise = if rotateClockwise then LeftF else RightF
rotatePlayer LeftF rotateClockwise = if rotateClockwise then UpF else DownF

getOffSet :: Facing -> Position
getOffSet UpF = (-1, 0)
getOffSet DownF = (1, 0)
getOffSet LeftF = (0, -1)
getOffSet RightF = (0, 1)

getFacingNum :: Facing -> Int
getFacingNum UpF = 3
getFacingNum DownF = 1
getFacingNum LeftF = 2
getFacingNum RightF = 0

getNextPos :: Player -> World -> Position
getNextPos ((pX, pY), facing) world = do
    let offset@(oX, oY) = getOffSet facing
    let nextPos = (pX + oX, pY + oY)

    let leftPos = Map.foldrWithKey (\(x,y) v (x', y') -> if x == pX && y < y' then (x,y) else (x', y')) (1000000,1000000) world
    let rightPos = Map.foldrWithKey (\(x,y) v (x', y') -> if x == pX && y > y' then (x,y) else (x', y')) (-1,-1) world
    let upPos = Map.foldrWithKey (\(x,y) v (x', y') -> if y == pY && x < x' then (x,y) else (x', y')) (1000000,1000000) world
    let downPos = Map.foldrWithKey (\(x,y) v (x', y') -> if y == pY && x > x' then (x,y) else (x', y')) (-1,-1) world

    if Map.member nextPos world then nextPos else if offset == (0, -1) then rightPos else if offset == (0, 1) then leftPos else if offset == (-1, 0) then downPos else upPos

applyOrders :: Player -> [Order] -> World -> Player
applyOrders player [] world = player
applyOrders (pos, facing) (RotateClockwise:rest) world = applyOrders (pos, rotatePlayer facing True) rest world
applyOrders (pos, facing) (RotateCounterClockwise:rest) world = applyOrders (pos, rotatePlayer facing False) rest world
applyOrders (pos, facing) (Move x:rest) world = do
    let (nx, ny) = getNextPos (pos, facing) world
    let curCellType = world Map.! (nx, ny)
    case curCellType of
        Empty -> applyOrders ((nx, ny), facing) ((if x == 1 then [] else [Move (x-1)]) ++ rest) world
        Wall -> applyOrders (pos, facing) rest world

data Facing = UpF | DownF | LeftF | RightF  deriving(Show, Eq)
type Player = (Position, Facing)
data CellType = Empty | Wall deriving(Show, Eq)
type Position = (Int, Int)
type Cell = (Position, CellType)
data Order = Move Int | RotateClockwise | RotateCounterClockwise deriving(Show, Eq)
type World = Map.Map Position CellType

parseCellLine :: Int -> Int -> String -> [Cell]
parseCellLine x y [] = []
parseCellLine x y (' ':rest) = parseCellLine x (y+1) rest
parseCellLine x y ('.':rest) = ((x, y), Empty) : parseCellLine x (y+1) rest
parseCellLine x y ('#':rest) = ((x, y), Wall) : parseCellLine x (y+1) rest

transformOrderLeft :: [String] -> [Order]
transformOrderLeft [x] = [Move (read x :: Int)]
transformOrderLeft (x:rest) = [Move (read x :: Int), RotateCounterClockwise] ++ transformOrderLeft rest

concatOrders :: [[Order]] -> [Order]
concatOrders [] = []
concatOrders [a] = a
concatOrders (a:b:rest) = a ++ [RotateClockwise] ++ concatOrders (b : rest)

main :: IO()
main = do 
    handle <- openFile "day_22.txt" ReadMode 
    content <- hGetContents handle
    let lines = wordsWhen (=='\n') content
    let orders = concatOrders $ (map) transformOrderLeft $ map (wordsWhen (=='L')) $ wordsWhen (=='R') $ last lines
    let worldList = concat $ map (\(x, s) -> parseCellLine x 0 s) $ zip [0..] $ init lines
    let world = Map.fromList worldList
    let initialPlayer = (fst $ head worldList, RightF)
    --print(initialPlayer)
    --print(orders)
    --print(world)
    let ((fX, fY), facing) = applyOrders initialPlayer orders world
    let score = 1000 * (fX+1) + 4 * (fY+1) + getFacingNum facing
    print(score)
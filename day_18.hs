module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode))
import Data.List (sort, group, groupBy, sortBy, find)
import Debug.Trace
import Data.Function (fix)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

data Cell = Obstidian | Air | Void deriving(Show, Eq)
type Position3D = (Int, Int, Int) 
type World = Map Position3D Cell

parseTuple :: String -> Position3D
parseTuple line = do
    let nums = wordsWhen (==',') line
    (read (nums !! 0) :: Int, read (nums !! 1) :: Int, read (nums !! 2) :: Int)

countFreeSides :: [Position3D] -> World -> Int
countFreeSides [] world = 0
countFreeSides ((x,y,z):rest) world = do
    let toCheckPos = [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)]
    let blockedSides = length $ filter (==False) $ map (\x -> Map.member x world) toCheckPos
    blockedSides + (countFreeSides rest world)

countFreeSidesP2 :: [Position3D] -> World -> Int
countFreeSidesP2 [] world = 0
countFreeSidesP2 ((x,y,z):rest) world = do
    let toCheckPos = [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)]
    let blockedSides = length $ filter (==True) $ map (\x -> Map.member x world && world Map.! x /= Void && world Map.! x /= Obstidian) toCheckPos
    blockedSides + (countFreeSidesP2 rest world)

-- close to max x,y or z of data (find with approximate reading of txt file)
magic = 22

isCloseToAir :: Position3D -> World -> Bool
isCloseToAir (x,y,z) world = do
    let off = [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)]
    (Map.size $ Map.filterWithKey (\k v -> v == Air && k `elem` off) world) >= 1

propagateAir :: World -> World
propagateAir world = do
    let newWorld = Map.mapWithKey (\k v -> if v == Void && isCloseToAir k world then Air else v) world
    if world == newWorld then world else trace ("prop: " ++ show(Map.size $ Map.filter (==Void) newWorld)) propagateAir newWorld

edges = Map.fromList $ [((x,y,z), Air) | x<-[0..magic], y<-[0,magic], z<-[0,magic]] ++ [((x,y,z), Air) | x<-[0,magic], y<-[0..magic], z<-[0,magic]] ++ [((x,y,z), Air) | x<-[0,magic], y<-[0,magic], z<-[0..magic]]

main :: IO()
main = do
    handle <- openFile "day_18.txt" ReadMode 
    content <- hGetContents handle
    let lines = wordsWhen (=='\n') content
    let positions3D = map parseTuple lines
    let world = Map.fromList $ map (\l -> (parseTuple l, Obstidian)) lines
    let p1Val = countFreeSides positions3D world
    let voidCells = [((x,y,z), Void) | x <- [0..magic], y <- [0..magic],z <- [0..magic]]
    let airWorld = propagateAir $ Map.union (Map.union world edges) (Map.fromList voidCells)
    let p2Wolrd = Map.union world (Map.filter (==Void) airWorld)
    print(p1Val)
    print(countFreeSides positions3D p2Wolrd)
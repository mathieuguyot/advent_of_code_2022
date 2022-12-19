module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode))
import Data.List (sort, group, groupBy, sortBy, find)
import Debug.Trace
import Data.Function (fix)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Arena = Set Position
type Position = (Int, Int)
data Jet = RightJet | LeftJet deriving(Show, Eq)

getArenaHeight :: Arena -> Int
getArenaHeight arena = Set.foldr (\(x,y) maxY -> maximum([y, maxY])) 0 arena

createShape :: Int -> Int -> [Position]
createShape turn height 
    | turn `mod` 5 == 1 = [(x, height) | x <- [2,3,4,5]]
    | turn `mod` 5 == 2 = [(3, height), (2, height+1), (3, height+1), (4, height+1), (3, height+2)]
    | turn `mod` 5 == 3 = [(2, height), (3, height), (4, height), (4, height+1), (4, height+2)]
    | turn `mod` 5 == 4 = [(2, height), (2, height+1), (2, height+2), (2, height+3)]
    | turn `mod` 5 == 0 = [(2, height), (3, height), (2, height+1), (3, height+1)]

shapeCollide :: [Position] -> Arena -> Bool
shapeCollide shape arena = foldr (\(x,y) b -> b || not (x >= 0 && x <= 6 && not (Set.member (x,y) arena))) False shape

pushShape :: Jet -> [Position] -> Arena -> [Position]
pushShape jet shape arena = do 
    let newShape = map (\(x,y) -> if jet == LeftJet then (x-1,y) else (x+1,y)) shape
    if shapeCollide newShape arena then shape else newShape

dropShape :: [Position] -> Arena -> ([Position], Bool)
dropShape shape arena = do 
    let newShape = map (\(x,y) -> (x,y-1)) shape
    if shapeCollide newShape arena then (shape, True) else (newShape, False) 

initArena :: Arena
initArena = Set.fromList [(x,0) | x <- [0..6]]

playRock :: [Jet] -> [Position] -> Arena -> (Arena, [Jet])
playRock jets shape arena = do
    let shapeAfterPush = pushShape (head jets) shape arena
    let (shapeAfterDrop, collide) = dropShape shapeAfterPush arena
    if collide then (Set.union arena (Set.fromList shapeAfterPush), tail jets) else playRock (tail jets) shapeAfterDrop arena

sanitizeArena :: Arena -> Arena
sanitizeArena arena = do
    let height = getArenaHeight arena
    Set.filter (\(x,y) -> (height - y) <= 50) arena

play :: Int -> [Jet] -> Arena -> Arena
play 2023 jets arena = arena
play turn jets arena = do
    let height = getArenaHeight arena
    let shape = createShape turn (height + 4)
    let (newArena, newJets) = playRock jets shape arena
    trace (show (turn)) play (turn+1) newJets (sanitizeArena newArena)

main :: IO()
main = do 
    handle <- openFile "day_17.txt" ReadMode 
    content <- hGetContents handle
    let jets = map (\c -> if c == '>' then RightJet else LeftJet) content
    let cyclicsJets = fix (jets ++)
    print(getArenaHeight $ play 1 cyclicsJets initArena)
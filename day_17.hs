module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode))
import Data.List (sort, group, groupBy, sortBy, find, findIndex)
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


findCycle :: Eq a => [a] -> ([a],[a])
findCycle xxs = fCycle xxs xxs
  where fCycle (x:xs) (_:y:ys)
         | x == y              = fStart xxs xs
         | otherwise           = fCycle xs ys
        fCycle _      _        = (xxs,[]) -- not cyclic
        fStart (x:xs) (y:ys)
         | x == y              = ([], x:fLength x xs)
         | otherwise           = let (as,bs) = fStart xs ys in (x:as,bs)
        fLength x (y:ys)
         | x == y              = []
         | otherwise           = y:fLength x ys

play :: Int -> [Jet] -> ([Int], Arena) -> ([Int], Arena)
play 2023 jets (heights, arena) = (heights, arena)
play turn jets (heights, arena) = do
    let height = getArenaHeight arena
    let shape = createShape turn (height + 4)
    let (newArena, newJets) = playRock jets shape arena
    let newHeight = getArenaHeight newArena
    
    play (turn+1) newJets (heights ++ [newHeight - height], newArena)

patternSize = 40
findPattern :: [Int] -> [Int] -> Bool
findPattern patternToCheck [] = False
findPattern patternToCheck l = do 
    let curPattern = take patternSize l
    if patternToCheck == curPattern then True else findPattern patternToCheck (tail l)

main :: IO()
main = do 
    handle <- openFile "day_17.txt" ReadMode 
    content <- hGetContents handle
    let jets = map (\c -> if c == '>' then RightJet else LeftJet) content
    let cyclicsJets = fix (jets ++)
    --print(fst $ play 1 cyclicsJets ([], initArena))
    let c = [0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4]
    let l = [1,3,2,1,2,1,3,2,2,0,1,3,2,0,2,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3,2,2,0,0,2,3,4,0,1,2,1,2,0,1,2,1,2,0,1,3,2,0,0,1,3,3,4,0,1,2,3,0,1,1,3]
    print(findPattern (take patternSize c) (drop patternSize c))
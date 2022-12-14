module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List (sort, group)
import Debug.Trace
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

data CellType = Rock | Sand deriving (Show, Read, Eq, Ord)
type Position = (Int, Int)
type Cell = (Position, CellType)
type CellMap = Map.Map Position CellType
type Trace = [Position]

-- BEGIN PARSING SECTION
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

evens [] = []
evens [x] = [x]
evens (e1:e2:xs) = e1 : evens xs

parsePosition :: String -> Position
parsePosition s = do
    let items = wordsWhen (==',') s
    (read (items !! 0) :: Int, read (items !! 1) :: Int)

parseTrace :: String -> Trace
parseTrace d = do
    let s = evens $ words d
    map parsePosition s
-- END PARSING SECTION

-- BEGIN INIT WORLD SECTION
getRocks :: Trace -> [Cell]
getRocks [] = []
getRocks [x] = []
getRocks ((ax,ay):(bx,by):rest) = [((x,y), Rock) | x <- [minimum([ax,bx])..maximum([ax,bx])], y <- [minimum([ay, by])..maximum([ay, by])]] ++ getRocks ((bx,by) : rest)

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

createWorld :: [Trace] -> [Cell]
createWorld [] = []
createWorld (t:rest) = rmdups (getRocks t ++ createWorld rest)

generateFloor :: Int -> [Cell]
generateFloor floorY = [((x,floorY+2), Rock) | x <- [-100000..100000]]
-- END INIT WORLD SECTION

-- BEGIN COMPUTATION SECTION
addSand :: Position -> CellMap -> CellMap
addSand (_, 1000) world = world
addSand (sx, sy) world = do
    let isBottomBlocked = Map.member (sx, sy+1) world
    let isLeftDiagBlocked = Map.member (sx-1, sy+1) world
    let isRightDiagBlocked = Map.member (sx+1, sy+1) world
    if not isBottomBlocked then addSand (sx, sy+1) world else 
        if not isLeftDiagBlocked then addSand (sx-1, sy+1) world else
            if not isRightDiagBlocked then addSand (sx+1, sy+1) world else
                Map.insert (sx, sy) Sand world

cycleSand :: CellMap -> CellMap
cycleSand world = do
    let newWorld = addSand (500,0) world
    if Map.size newWorld == Map.size world then world else cycleSand newWorld

sandCount :: CellMap -> Int
sandCount world = Map.size $ Map.filter (\v -> v == Sand) world
-- END COMPUTATION SECTION

main :: IO()
main = do 
    handle <- openFile "day_14.txt" ReadMode 
    content <- hGetContents handle
    let lines = wordsWhen (=='\n') content
    let traces = map parseTrace lines
    let world = Map.fromList (createWorld traces)
    let highestY =  Map.foldrWithKey (\(x,y) _ maxY -> maximum([y, maxY])) 0 world
    let worldWithFloor = Map.fromList ((createWorld traces) ++ (generateFloor highestY))
    let worldBeforeAbyss = cycleSand world
    let worldWithFloorBeforeAbyss = cycleSand worldWithFloor
    print(sandCount worldBeforeAbyss)
    print(sandCount worldWithFloorBeforeAbyss)

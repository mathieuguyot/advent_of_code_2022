module Day8 where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List (sort, intersect, intercalate, group)
import Debug.Trace

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

type Position = (Int, Int)
type Offset = (Int, Int)
data Rope = Rope [Position] [Position] deriving (Show, Eq)
data Move = Move Offset Int deriving (Show, Eq)-- offset, count

getTailNewPos :: Offset -> Position -> Position
getTailNewPos (ox, oy) (tx, ty)
    | ox > 1 || oy > 1 || ox < -1 || oy < -1 = do
        let (v,r) = quotRem ox 2
        let (v2,r2) = quotRem oy 2
        (tx -(v+r), ty-(v2+r2))
    | otherwise = (tx, ty)

performMoves :: Rope -> [Move] -> Rope
performMoves r [] = r
performMoves r (x:xs) = performMoves (move r x) xs

moveR :: Position -> [Position] -> [Position]
moveR (hx, hy) [] = []
moveR (hx, hy) ((tx, ty):otherTails) = do
    let (ntx, nty) = getTailNewPos (tx - hx, ty - hy) (tx, ty)
    [(ntx, nty)] ++ moveR (ntx, nty) otherTails

move :: Rope -> Move -> Rope
move rope (Move _ 0) = rope
move (Rope ((hx, hy):otherTails) visitedTailPos) (Move (ox, oy) count) = do
    let (nhx, nhy) = (hx + ox, hy + oy)
    let newPos = [(nhx, nhy)] ++ moveR (nhx, nhy) otherTails
    let newRope = Rope newPos (visitedTailPos ++ [last newPos])
    move newRope (Move (ox, oy) (count - 1))

getOffSet :: String -> Offset
getOffSet "R" = (1,0)
getOffSet "L" = (-1,0)
getOffSet "U" = (0,1)
getOffSet "D" = (0,-1)

parseLine :: String -> Move
parseLine line = do
    let items = words line
    let offset = getOffSet (items !! 0)
    let count =  read (items !! 1) :: Int
    Move offset count

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

main :: IO()
main = do
    handle <- openFile "day_09.txt" ReadMode
    content <- hGetContents handle
    let lines = wordsWhen (== '\n') content
    let moves = map parseLine lines
    let rope2 = Rope [(0,0), (0,0)] []
    let rope10 = Rope [(0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0)] []
    let (Rope knots10 visited10) = performMoves rope10 moves
    let (Rope knots2 visited2) = performMoves rope2 moves
    print(length $ rmdups $ visited2)
    print(length $ rmdups $ visited10)

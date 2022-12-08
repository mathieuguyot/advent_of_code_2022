module Day8 where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List.Split (splitOn)
import Data.List (sort, intersect, intercalate)
import Data.Map as M
import Debug.Trace

type Position = (Int, Int)
type Cell = (Position, Int)

getDirValues :: (Position -> Position) -> Position -> [Int] -> M.Map Position Int -> [Int]
getDirValues updater pos values cells = do
    let value = M.lookup pos cells
    case value of
     Just existingVal -> getDirValues updater (updater pos) (values ++ [existingVal]) cells
     Nothing -> values

isVisible :: Position -> Int -> M.Map Position Int -> Bool
isVisible (x,y) val cells = do
    let topValues = getDirValues (\(x,y) -> (x,y+1)) (x,y+1) [] cells
    let bottomValues = getDirValues (\(x,y) -> (x,y-1)) (x,y-1) [] cells
    let leftValues = getDirValues (\(x,y) -> (x+1,y)) (x+1,y) [] cells
    let rightValues = getDirValues (\(x,y) -> (x-1,y)) (x-1,y) [] cells
    let topValid = (length topValues) == 0 || maximum topValues < val
    let bottomValid = (length bottomValues) == 0 || maximum bottomValues < val
    let leftValid = (length leftValues) == 0 || maximum leftValues < val
    let rightValid = (length rightValues) == 0 || maximum rightValues < val
    topValid || bottomValid || leftValid || rightValid

getView :: Int -> [Int] -> [Int]
getView val [] = []
getView val (x:xs) = if x >= val then [val] else [val] ++ getView val xs
 
getScenicScore :: Position -> Int -> M.Map Position Int -> Int
getScenicScore (x,y) val cells = do
    let topValues = getDirValues (\(x,y) -> (x,y+1)) (x,y+1) [] cells
    let bottomValues = getDirValues (\(x,y) -> (x,y-1)) (x,y-1) [] cells
    let leftValues = getDirValues (\(x,y) -> (x+1,y)) (x+1,y) [] cells
    let rightValues = getDirValues (\(x,y) -> (x-1,y)) (x-1,y) [] cells
    let topView = length $ getView val topValues
    let bottomView = length $ getView val bottomValues
    let leftView = length $ getView val leftValues
    let rightView = length $ getView val rightValues
    topView * bottomView * leftView * rightView

parseStrWorld :: [String] -> [Cell]
parseStrWorld world = parse' world 0
    where parse' :: [String] -> Int -> [Cell]
          parse' [] _ = []
          parse' (x:xs) index = [((index, y), read [x !! y]:: Int) | y <- [0..(length x - 1)]] ++ parse' xs (index + 1)

main :: IO()
main = do
    handle <- openFile "day_08.txt" ReadMode
    content <- hGetContents handle
    let world = M.fromList $ parseStrWorld $ splitOn "\n" content
    print(length $ M.filterWithKey (\pos val -> isVisible pos val world) world)
    print(maximum $ M.mapWithKey (\pos val -> getScenicScore pos val world) world)
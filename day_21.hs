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

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

parseOperation :: String -> Monkey
parseOperation s = do
    let w = words s
    let name = init $ w !! 0
    let operator =  w !! 2
    let value = if operator == "*" then Time (w !! 1) (w !! 3) else if operator == "-" then Sub (w !! 1) (w !! 3) else if operator == "+" then Add (w !! 1) (w !! 3) else Div (w !! 1) (w !! 3)
    if length w == 2 then (name, Literal (read (w !! 1) :: Integer)) else (name, value)

type MonkeyName = String
type Monkey = (MonkeyName, Operation)
data Operation = X | Literal Integer | Add MonkeyName MonkeyName | Sub MonkeyName MonkeyName | Time MonkeyName MonkeyName | Div MonkeyName MonkeyName deriving(Show, Eq) 

isLiteral :: Operation -> Bool
isLiteral (Literal v) = True
isLiteral _ = False

getLiteralValue :: Operation -> Integer
getLiteralValue (Literal v) = v

solve :: Monkey -> Map.Map MonkeyName Operation -> Map.Map MonkeyName Operation
solve (name, X) m = m
solve (name, Literal v) m = m
solve (name, Add n1 n2) m = if isLiteral (m Map.! n1) && isLiteral (m Map.! n2) then Map.insert name (Literal $ getLiteralValue (m Map.! n1) + getLiteralValue (m Map.! n2)) m else m
solve (name, Sub n1 n2) m = if isLiteral (m Map.! n1) && isLiteral (m Map.! n2) then Map.insert name (Literal $ getLiteralValue (m Map.! n1) - getLiteralValue (m Map.! n2)) m else m
solve (name, Time n1 n2) m = if isLiteral (m Map.! n1) && isLiteral (m Map.! n2) then Map.insert name (Literal $ getLiteralValue (m Map.! n1) * getLiteralValue (m Map.! n2)) m else m
solve (name, Div n1 n2) m = if isLiteral (m Map.! n1) && isLiteral (m Map.! n2) then Map.insert name (Literal $ getLiteralValue (m Map.! n1) `div` getLiteralValue (m Map.! n2)) m else m

solveCycle :: [MonkeyName] -> Map.Map MonkeyName Operation -> Map.Map MonkeyName Operation
solveCycle [] m = m
solveCycle (name:rest) m = do
    let op = m Map.! name
    solveCycle rest (solve (name, op) m)

solveUntil :: MonkeyName -> [MonkeyName] -> Map.Map MonkeyName Operation -> Integer
solveUntil name names m = do
    let newM = solveCycle names m
    if isLiteral (newM Map.! name) then getLiteralValue (newM Map.! name) else solveUntil name names newM

solveMax :: [MonkeyName] -> Map.Map MonkeyName Operation -> Map.Map MonkeyName Operation
solveMax names m = do
    let newM = solveCycle names m
    if newM == m then m else solveMax names newM 


useName :: MonkeyName -> Operation -> Bool
useName n (Add n1 n2) = n == n1 || n == n2
useName n (Sub n1 n2) = n == n1 || n == n2
useName n (Time n1 n2) = n == n1 || n == n2
useName n (Div n1 n2) = n == n1 || n == n2
useName _ _ = False

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "looser"

type MaybeInteger = Maybe Integer

isDiv :: Operation -> Bool
isDiv (Div a b) = True
isDiv _ = False

isTime :: Operation -> Bool
isTime (Time a b) = True
isTime _ = False

isSub :: Operation -> Bool
isSub (Sub a b) = True
isSub _ = False

solveMustEqualItems :: (String, Integer) -> (String, Operation) -> (String, Operation) -> Map.Map MonkeyName Operation -> Map.Map String MaybeInteger -> Map.Map String MaybeInteger
solveMustEqualItems (fatherName, toEqual) (leftName, leftOp) (rightName, rightOp) m mustEqualM = do
    let fatherOp = m Map.! fatherName
    let n1 = leftName
    let n2 = rightName
    let v1Known = isLiteral (leftOp) || (mustEqualM Map.! leftName) /= Nothing
    let v2Known = isLiteral (rightOp) || (mustEqualM Map.! rightName) /= Nothing
    let v1Value = if isLiteral leftOp then getLiteralValue leftOp else fromJust (mustEqualM Map.! leftName)
    let v2Value = if isLiteral rightOp then getLiteralValue rightOp else fromJust (mustEqualM Map.! rightName)
    let deducedV1 = if isDiv fatherOp then (toEqual * v2Value) else if isTime fatherOp then (toEqual `div` v2Value) else if isSub fatherOp then (toEqual + v2Value) else (toEqual - v2Value)
    let deducedV2 = if isDiv fatherOp then (v1Value `div` toEqual) else if isTime fatherOp then (toEqual `div` v1Value) else if isSub fatherOp then (v1Value - toEqual) else (toEqual - v1Value)
    if v1Known && v2Known || (not v1Known && not v2Known) then mustEqualM else if not v1Known then Map.insert n1 (Just $ deducedV1) mustEqualM else Map.insert n2 (Just $ deducedV2) mustEqualM

getChildOp :: String -> Map.Map MonkeyName Operation -> [(String, Operation)]
getChildOp n m = do
    let op = m Map.! n
    case op of 
        X -> []
        Literal v -> []
        Add n1 n2 -> [(n1, m Map.! n1), (n2, m Map.! n2)]
        Time n1 n2 -> [(n1, m Map.! n1), (n2, m Map.! n2)]
        Sub n1 n2 -> [(n1, m Map.! n1), (n2, m Map.! n2)]
        Div n1 n2 -> [(n1, m Map.! n1), (n2, m Map.! n2)]

solveMustEqual :: (String, MaybeInteger) -> Map.Map MonkeyName Operation -> Map String MaybeInteger ->  Map String MaybeInteger
solveMustEqual (name, Nothing) m mustEqualM = mustEqualM
solveMustEqual (name, Just v) m mustEqualM = do
    let ops = getChildOp name m
    let newMustEqualM = if length ops == 2 then solveMustEqualItems (name, v) (ops!!0) (ops!!1) m mustEqualM else mustEqualM
    if newMustEqualM == mustEqualM then mustEqualM else solveMustEqual (name, Just v) m newMustEqualM

solveMustEqualAll :: [(String, MaybeInteger)] -> Map.Map MonkeyName Operation -> Map String MaybeInteger -> Map String MaybeInteger
solveMustEqualAll [] m mustEqualM = mustEqualM
solveMustEqualAll (x:rest) m mustEqualM = solveMustEqualAll rest m (solveMustEqual x m mustEqualM)

solveMaxEqualAll :: Map.Map MonkeyName Operation -> Map String MaybeInteger -> Map String MaybeInteger
solveMaxEqualAll m mustEqualM = do
    let newMustEqualM = solveMustEqualAll (filter (\(x,v) -> v /= Nothing) $ Map.toList mustEqualM) m mustEqualM
    if mustEqualM == newMustEqualM then newMustEqualM else solveMaxEqualAll m newMustEqualM

main = do
    print("DAY 21")
    handle <- openFile "day_21.txt" ReadMode 
    content <- hGetContents handle
    let monkeys = map parseOperation $ wordsWhen (=='\n') content
    let monkeyNames = map fst monkeys
    print(solveUntil "root" monkeyNames (Map.fromList monkeys))

    let noHumanM = Map.insert "humn" (X) (Map.fromList monkeys)
    let solvedMaxM = solveMax monkeyNames noHumanM
    -- Just 22931068684876 is the value read in solvedMaxM at root child top level and tlpd inject manually bcs i'm lazy today
    let toFind = Map.fromList $ map (\x -> if x == "tlpd" then (x, Just 22931068684876) else (x, Nothing)) $ map fst $ Map.toList $ Map.filterWithKey (\k v -> not (isLiteral v) && k /= "root") $ solvedMaxM
    print(filter (\(n,v) -> n == "humn") $ Map.toList (solveMaxEqualAll solvedMaxM toFind))
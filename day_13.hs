{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List (sort, sortBy)
import Debug.Trace
import Data.Text (replace, pack, unpack)

data Signal = Regular Int | Nested [Signal] deriving (Show, Read, Eq)
type SignalPair = (Signal, Signal)

makePairs :: [Signal] -> [SignalPair]
makePairs [] = []
makePairs (a:b:xs) = [(a,b)] ++ makePairs xs

strToSignal :: String -> Signal
strToSignal str = read (unpack $ replace "Regular 1Regular 0" "Regular 10" $ pack $ concatMap (\e -> if e == '[' then "Nested [" else if isDigit e then "Regular " ++ [e] else [e]) str) :: Signal

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

isSignalValid :: SignalPair -> Maybe Bool
isSignalValid (Regular a, Regular b) = if a == b then Nothing else Just (b > a)
isSignalValid (Regular a, Nested b) = isSignalValid (Nested [Regular a], Nested b)
isSignalValid (Nested a, Regular b) = isSignalValid (Nested a, Nested [Regular b])
isSignalValid (Nested [], Nested []) = Nothing
isSignalValid (Nested [], Nested (b)) = Just True
isSignalValid (Nested (a), Nested []) = Just False
isSignalValid (Nested (a:as), Nested (b:bs)) = do
    let isValid = isSignalValid (a,b)
    case isValid of
        Nothing -> isSignalValid (Nested as, Nested bs) 
        Just b -> Just b

sumIndices :: Int -> [Maybe Bool] -> Int
sumIndices _ [] = 0
sumIndices i (Just True:x) = i + sumIndices (i+1) x
sumIndices i (Just False:x) = sumIndices (i+1) x

sortSignals a b = do
    let isValid = isSignalValid (a, b)
    case isValid of
        Just True -> LT
        Just False -> GT

timesDecoderKey :: Int -> [Signal] -> Int
timesDecoderKey n [] = 1
timesDecoderKey i (Nested[Nested[Regular 2]]:x) = i * timesDecoderKey (i+1) x
timesDecoderKey i (Nested[Nested[Regular 6]]:x) = i * timesDecoderKey (i+1) x
timesDecoderKey i (a:x) = timesDecoderKey (i+1) x

main :: IO()
main = do 
    handle <- openFile "day_13.txt" ReadMode 
    content <- hGetContents handle
    let signals = map strToSignal $ wordsWhen (== '\n') content
    let signalsPairs = makePairs signals
    let validityOfSignals = map isSignalValid signalsPairs
    print(sumIndices 1 validityOfSignals)
    let signalsP2 = [Nested[Nested[Regular 2]], Nested[Nested[Regular 6]]] ++ signals
    print(timesDecoderKey 1 $ sortBy sortSignals signalsP2)

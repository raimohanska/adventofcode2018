{-# LANGUAGE TupleSections, LambdaCase, FlexibleContexts #-}
module Main where
import Text.Parsec
import Data.Char(digitToInt, toLower, toUpper, isSpace)
import Data.List(tails, foldl', sort, group, intersect)
import Data.Maybe(maybeToList, listToMaybe, fromMaybe)
import Data.Either(rights)
import GHC.Exts(sortWith, groupWith)
import qualified Data.Set as S
import Debug.Trace(traceShow)

main = putStrLn $ show $ solve 424 71144

--debug = traceShow
debug x y = y

-- TODO: using a singly-linked list is simply too slow! Need another data structure!

solve playerCount lastValue = 
  let scores = take playerCount $ repeat 0
      circle = []
      marbles = [0..lastValue]
      result = foldl process (scores, circle) marbles
      finalScores = fst result :: [Int]
      topScore = debug ("scores " ++ (show $ finalScores)) $ foldl max 0 finalScores
  in topScore

process (scores, []) marble = (scores, [marble])
process (scores, circle) marble 
  | marble > 0 && marble `mod` 23 == 0 = 
    let (toRemove:left) = rotateBack 7 circle
        scoreToAdd = marble + toRemove
    in debug ("adding " ++ (show scoreToAdd)) $ (incrementScore scoreToAdd scores, left)
  | otherwise = debug ("scores " ++ show (take 1 scores)) $ (incrementScore 0 scores, marble : rotate 2 circle)

incrementScore increment (x:xs) = xs ++ [x + increment]

rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

rotateBack _ [] = []
rotateBack n xs = drop (length xs - n) xs ++ take (length xs - n) xs

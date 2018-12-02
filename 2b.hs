{-# LANGUAGE OverloadedStrings, LambdaCase, TupleSections #-}
module Main where

import Data.List(sort, group)
import qualified Data.Set as S

main = do
  text <- readFile "2.txt"
  let solution = solve $ lines text
  putStrLn $ show solution

solve lines = 
  let (a, b) = head $ filter (\pair -> (uncurry countDiff) pair == 1) $ pairs lines
  in commonLetters a b

countDiff a b = length $ filter (uncurry (/=)) $ zip a b

commonLetters a b = map fst $ filter (uncurry (==)) $ zip a b

pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

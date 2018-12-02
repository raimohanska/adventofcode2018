{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import Data.List(sort, group)
import qualified Data.Set as S

main = do
  text <- readFile "2.txt"
  let solution = solve $ lines text
  putStrLn $ show solution

solve lines = (countCounts 2 lines) * (countCounts 3 lines)

countCounts count strs = length $ filter (hasCount count) strs
hasCount count str = elem count $ map length $ (group . sort) str

{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import Data.List(group)
import qualified Data.Set as S

main = do
  text <- readFile "1.txt"
  let solution = solve $ map readNum $ lines text
  putStrLn $ show solution

solve xs = solveRecursive (cycle xs) 0 S.empty

solveRecursive (x:xs) curr found | S.member curr found = curr
                                 | otherwise = solveRecursive xs (curr+x) (S.insert curr found)

readNum :: String -> Int
readNum ('+' : rest) = readNum rest
readNum str = read str

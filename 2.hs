{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import Data.List(group)
import qualified Data.Set as S

main = do
  text <- readFile "1.txt"
  let solution = solve $ map readNum $ lines text
  putStrLn $ show solution

solve xs =
  let frequencies = scanl (+) 0 $ cycle xs
      foundSoFar = scanl (\found next -> S.insert next found) S.empty frequencies
      withPreviouslyFound = tail frequencies `zip` foundSoFar
      repeated = map fst $ filter (uncurry S.member) withPreviouslyFound
  in head repeated

readNum :: String -> Int
readNum ('+' : rest) = readNum rest
readNum str = read str

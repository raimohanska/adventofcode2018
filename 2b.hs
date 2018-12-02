{-# LANGUAGE TupleSections, LambdaCase #-}
module Main where

import Data.List(tails)

main = readFile "2.txt" >>= (return . solve . lines) >>= print

solve = commonLetters . head . filter ((1 ==) . countDiff) . pairs

countDiff (a, b) = length $ filter (uncurry (/=)) $ zip a b

commonLetters (a, b) = map fst $ filter (uncurry (==)) $ zip a b

pairs xs = xs `zip` (tails $ tail xs) >>= (\(x, ys) -> map (x,) ys)

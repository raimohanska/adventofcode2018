{-# LANGUAGE TupleSections, LambdaCase, FlexibleContexts #-}
module Main where
import Text.Parsec
import Data.Char(digitToInt, toLower, isSpace)
import Data.List(tails, foldl', sort, group, intersect)
import Data.Maybe(maybeToList, listToMaybe, fromMaybe)
import Data.Either(rights)
import GHC.Exts(sortWith, groupWith)
import qualified Data.Set as S

main = readFile "5.txt" >>= (return . solve . removeWhitespace) >>= print

solve input = length $ react isOpposite "" input

removeWhitespace = filter (not . isSpace)

react :: (a -> a -> Bool) -> [a] -> [a] -> [a]
react f xs [] = reverse xs
react f [] (x:xs) = react f [x] xs
react f (x:xs) (y:ys) | f x y = react f xs ys
                    | otherwise = react f (y : x : xs) ys

isOpposite a b = (a /= b) && (toLower a == toLower b)

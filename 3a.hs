{-# LANGUAGE TupleSections, LambdaCase, FlexibleContexts #-}
module Main where
import Text.Parsec
import Data.Char(digitToInt)
import Data.List(tails, foldl', sort, group)
import Data.Either(rights)

main = readFile "3.txt" >>= (return . solve . lines) >>= print

solve lines = 
  let
    squares = rights $ map (parse square "") lines
    covered = group $ sort $ (squares >>= coveredCoords)
    overCovered = filter ((>1) . length) covered
  in length overCovered

type Coords = (Int, Int)
coveredCoords :: Square -> [Coords]
coveredCoords square = [ (x, y) | x <- exclRange _x _width square, y <- exclRange _y _height square]

exclRange :: (a -> Int) -> (a -> Int) -> a -> [Int]
exclRange getStart getLength src = [getStart src .. (getStart src + getLength src - 1)]

data Square = Square { _id :: Int, _x :: Int, _y :: Int, _width :: Int, _height :: Int } deriving (Eq, Show)

square = Square 
  <$> (char '#' >> int) 
  <*> (string " @ " >> int) 
  <*> (char ',' >> int)
  <*> (string ": " >> int) 
  <*> (char 'x' >> int)

int = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

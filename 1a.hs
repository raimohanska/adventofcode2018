module Main where

main = do
  text <- readFile "1.txt"
  let total = sum $ map readNum $ lines text
  putStrLn $ show total

readNum :: String -> Int
readNum ('+' : rest) = readNum rest
readNum str = read str

module DayThree where

import Data.Char (digitToInt)

readInput :: IO [[Int]]
readInput = do
    contents <- readFile "inputs/DayThree.txt"
    pure $ map digitToInt <$> lines contents

findMaxRest :: Int -> [Int] -> (Int, [Int])
findMaxRest n (x : xs)
    | length xs < n = (0, [])
    | x >= fst maxRest = (x, xs)
    | otherwise = maxRest
  where
    maxRest :: (Int, [Int])
    maxRest = findMaxRest n xs
findMaxRest _ [] = (0, [])

getMaxJoltage :: Int -> Int -> [Int] -> Int
getMaxJoltage accum n xs
    | n == 1 = 10 * accum + maximum xs
    | otherwise = getMaxJoltage (10 * accum + y) (pred n) ys
  where
    (y, ys) = findMaxRest (pred n) xs

partOne :: [[Int]] -> Int
partOne = sum . map (getMaxJoltage 0 2)

partTwo :: [[Int]] -> Int
partTwo = sum . map (getMaxJoltage 0 12)

dayThree :: IO ()
dayThree = do
    input <- readInput
    putStrLn $ "Part One answer: " ++ show (partOne input)
    putStrLn $ "Part Two answer: " ++ show (partTwo input)

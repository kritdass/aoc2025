module DaySix where

import Data.List (transpose)
import Data.List.Split (splitWhen)

readInput :: IO [String]
readInput = do
    contents <- readFile "inputs/DaySix.txt"
    pure $ lines contents

partOne' :: ([[Int]], [[Int] -> Int]) -> Int
partOne' (nums, opp : opps) = opp (head <$> nums) + partOne' (tail <$> nums, opps)
partOne' (_, []) = 0

partOne :: [String] -> Int
partOne xs = partOne' (map read <$> init xs', (\o -> if o == "+" then sum else product) <$> last xs')
  where
    xs' = words <$> xs

partTwo :: [String] -> Int
partTwo xs = sum $ process <$> fixed
  where
    fixed :: [[String]]
    fixed = splitWhen (null . words) (transpose xs)
    process :: [String] -> Int
    process (x : rest) = (if last x == '+' then sum else product) (read <$> init x : rest)
    process [] = 0

daySix :: IO ()
daySix = do
    input <- readInput
    putStrLn $ "Part One answer: " ++ show (partOne input)
    putStrLn $ "Part Two answer: " ++ show (partTwo input)

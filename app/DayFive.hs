module DayFive where

import Data.List (sort)
import Data.List.Split (splitOn)

readInput :: IO ([(Int, Int)], [Int])
readInput = do
    contents <- readFile "inputs/DayFive.txt"
    case splitOn "\n\n" contents of
        [rangesBlock, ingredientsBlock] ->
            pure (toPairs . splitOn "-" <$> lines rangesBlock, read <$> lines ingredientsBlock)
        _ -> error "Bad format"
  where
    toPairs :: [String] -> (Int, Int)
    toPairs [a, b] = (read a, read b)
    toPairs _ = error "Bad Format"

partOne :: ([(Int, Int)], [Int]) -> Int
partOne (ranges, ingredients) =
    length
        . filter
            ( \ingredient ->
                any (\(a, b) -> a <= ingredient && ingredient <= b) ranges
            )
        $ ingredients

partTwo' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
partTwo' xs@((a, b) : rest) ((a', b') : xs')
    | b < a' = partTwo' ((a', b') : xs) xs'
    | b' <= b = partTwo' xs xs'
    | otherwise = partTwo' ((a, b') : rest) xs'
partTwo' [] ((a, b) : xs) = partTwo' [(a, b)] xs
partTwo' merged [] = merged

partTwo :: ([(Int, Int)], [Int]) -> Int
partTwo (ranges, _) = sum . map (succ . uncurry (flip (-))) $ partTwo' [] (sort ranges)

dayFive :: IO ()
dayFive = do
    input <- readInput
    putStrLn $ "Part One answer: " ++ show (partOne input)
    putStrLn $ "Part Two answer: " ++ show (partTwo input)

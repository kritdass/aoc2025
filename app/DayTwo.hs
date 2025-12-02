module DayTwo where

import Data.List.Split (splitOn)

readInput :: IO [(String, String)]
readInput = do
    contents <- readFile "inputs/DayTwo.txt"
    pure $ toPairs . splitOn "-" <$> splitOn "," contents
  where
    toPairs :: [String] -> (String, String)
    toPairs [a, b] = (a, b)
    toPairs _ = error "Bad Format"

numDigits :: Int -> Int
numDigits = succ . floor . logBase (10 :: Float) . fromIntegral

repeated :: Int -> Int -> Int
repeated k x = mod x (10 ^ (numDigits x `div` k))

multiplier :: Int -> Int -> Int
multiplier k x = (10 ^ l - 1) `div` (10 ^ (l `div` k) - 1)
  where
    l :: Int
    l = numDigits x

isInvalid :: Int -> String -> Bool
-- isInvalid k x = mod (numDigits x) k == 0 && x == multiplier k x * repeated k x
isInvalid k x = mod l k == 0 && x == (concat . replicate k . take (l `div` k)) x
  where
    l :: Int
    l = length x

partOne :: [(String, String)] -> Int
partOne = sum . map read . concatMap (filter (isInvalid 2) . (\(a, b) -> [show x | x <- [read a :: Int .. read b]]))

isInvalid' :: String -> Bool
isInvalid' x = any (`isInvalid` x) [k | k <- [2 .. l], l `mod` k == 0]
  where
    l :: Int
    l = length x

partTwo :: [(String, String)] -> Int
partTwo = sum . map read . concatMap (filter isInvalid' . (\(a, b) -> [show x | x <- [read a :: Int .. read b]]))

dayTwo :: IO ()
dayTwo = do
    input <- readInput
    putStrLn $ "Part One answer: " ++ show (partOne input)
    putStrLn $ "Part Two answer: " ++ show (partTwo input)

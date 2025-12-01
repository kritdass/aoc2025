module DayOne where

rotationToInt :: String -> Int
rotationToInt (x : xs) = sign x * read xs
  where
    sign :: Char -> Int
    sign a = if a == 'L' then -1 else 1
rotationToInt _ = error "Bad Format"

readInput :: IO [Int]
readInput = do
    contents <- readFile "inputs/DayOne.txt"
    pure $ rotationToInt <$> lines contents

partOne' :: Int -> Int -> [Int] -> Int
partOne' accum a (x : xs)
    | rotated == 0 = partOne' (succ accum) rotated xs
    | otherwise = partOne' accum rotated xs
  where
    rotated :: Int
    rotated = mod (a + x) 100
partOne' accum _ [] = accum

partOne :: [Int] -> Int
partOne = partOne' 0 50

partTwo' :: Int -> Int -> [Int] -> Int
partTwo' accum a (x : xs)
    | x > 0 = partTwo' (accum + div (a + x) 100) rotated xs
    | a == 0 && x < 0 = partTwo' (accum + div (abs x) 100) rotated xs
    | x <= -a = partTwo' (accum + div (abs (a + x)) 100 + 1) rotated xs
    | otherwise = partTwo' accum rotated xs
  where
    rotated :: Int
    rotated = mod (a + x) 100
partTwo' accum _ [] = accum

partTwo :: [Int] -> Int
partTwo = partTwo' 0 50

dayOne :: IO ()
dayOne = do
    input <- readInput
    putStrLn $ "Part One answer: " ++ show (partOne input)
    putStrLn $ "Part Two answer: " ++ show (partTwo input)

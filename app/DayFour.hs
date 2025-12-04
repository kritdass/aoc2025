module DayFour where

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

type VV a = Vector (Vector a)

readInput :: IO (VV Bool)
readInput = do
    contents <- readFile "inputs/DayFour.txt"
    pure $ V.map (V.map (== '@') . V.fromList) (V.fromList (lines contents))

accessible' :: Int -> [(Int, Int)] -> (Int, Int) -> VV Bool -> (Int, [(Int, Int)])
accessible' n acc (r, c) grid
    | r >= V.length grid = (n, acc)
    | c >= V.length (grid ! r) = accessible' n acc (r + 1, 0) grid
    | not $ (grid ! r) ! c = accessible' n acc (r, c + 1) grid
    | length (filter id adj) < 4 = accessible' (n + 1) ((r, c) : acc) (r, c + 1) grid
    | otherwise = accessible' n acc (r, c + 1) grid
  where
    adjPos :: [(Int, Int)]
    adjPos = [(r + a, c + b) | a <- [-1, 0, 1], b <- [-1, 0, 1], a /= 0 || b /= 0]
    getPos :: (Int, Int) -> Bool
    getPos (r', c')
        | r' < 0 || r' >= V.length grid = False
        | c' < 0 || c' >= V.length (grid ! r) = False
        | otherwise = (grid ! r') ! c'
    adj :: [Bool]
    adj = getPos <$> adjPos

accessible :: VV Bool -> (Int, [(Int, Int)])
accessible = accessible' 0 [] (0, 0)

partOne :: VV Bool -> Int
partOne = fst . accessible

remove :: VV Bool -> [(Int, Int)] -> VV Bool
remove grid ((r, c) : xs) = remove (grid // [(r, (grid ! r) // [(c, False)])]) xs
remove grid [] = grid

partTwo' :: Int -> VV Bool -> Int
partTwo' accum grid
    | n == 0 = accum
    | otherwise = partTwo' (accum + n) (remove grid acc)
  where
    (n, acc) = accessible grid

partTwo :: VV Bool -> Int
partTwo = partTwo' 0

dayFour :: IO ()
dayFour = do
    input <- readInput
    putStrLn $ "Part One answer: " ++ show (partOne input)
    putStrLn $ "Part Two answer: " ++ show (partTwo input)

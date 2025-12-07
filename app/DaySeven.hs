module DaySeven where

import Data.List
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

readInput :: IO [String]
readInput = do
    contents <- readFile "inputs/DaySeven.txt"
    pure $ lines contents

partOne' :: Int -> [Int] -> [String] -> Int
partOne' accum beams (row : grid) = partOne' (accum + length splits) beams' grid
  where
    splits :: [Int]
    splits = filter (`elem` beams) (elemIndices '^' row)
    beams' :: [Int]
    beams' =
        nub $
            concatMap
                ( filter (\beam -> 0 <= beam && beam < length row)
                    . (\beam -> if beam `elem` splits then [beam - 1, beam + 1] else [beam])
                )
                beams
partOne' accum _ [] = accum

partOne :: [String] -> Int
partOne (row : grid) = partOne' 0 (elemIndices 'S' row) grid
partOne [] = error "Bad format"

partTwo' :: Vector Int -> [Vector Char] -> Int
partTwo' routes (row : grid) = partTwo' routes' grid
  where
    calcNewRoutes :: Int -> Int
    calcNewRoutes i
        | row ! i == '^' = 0
        | otherwise =
            routes ! i
                + sum
                    [ routes ! a
                    | a <- [i - 1, i + 1]
                    , 0 <= a && a < V.length routes
                    , row ! a == '^'
                    ]
    routes' :: Vector Int
    routes' = V.generate (V.length routes) calcNewRoutes
partTwo' routes [] = V.sum routes

partTwo :: [String] -> Int
partTwo (row : grid) = case elemIndex 'S' row of
    Just i -> partTwo' (v // [(i, 1)]) (V.fromList <$> grid)
    Nothing -> error "Bad format"
  where
    v :: Vector Int
    v = V.replicate (length row) 0
partTwo [] = error "Bad format"

daySeven :: IO ()
daySeven = do
    input <- readInput
    putStrLn $ "Part One answer: " ++ show (partOne input)
    putStrLn $ "Part Two answer: " ++ show (partTwo input)

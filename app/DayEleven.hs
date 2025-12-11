module DayEleven where

import Data.List
import Data.List.Split (splitOn)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

readInput :: IO (Vector [Int], Map String Int)
readInput = do
    contents <- readFile "inputs/DayEleven.txt"
    let rawMap = M.fromList [(k, words v) | [k, v] <- map (splitOn ": ") (lines contents)]
    let allNames = nub $ M.keys rawMap ++ concat (M.elems rawMap)
    let key = M.fromList $ zip allNames [0 ..]
    let graph = V.fromList [map (key M.!) (M.findWithDefault [] name rawMap) | name <- allNames]
    pure (graph, key)

getPaths :: Vector [Int] -> Int -> Vector Int
getPaths graph target = paths
  where
    paths = V.generate (V.length graph) getPathCount
    getPathCount v
        | v == target = 1
        | otherwise = sum [paths ! w | w <- graph ! v]

pathsBetween :: Vector [Int] -> Int -> Int -> Int
pathsBetween graph start target = getPaths graph target ! start

partOne :: (Vector [Int], Map String Int) -> Int
partOne (graph, key) = pathsBetween graph you out
  where
    you = key M.! "you"
    out = key M.! "out"

partTwo :: (Vector [Int], Map String Int) -> Int
partTwo (graph, key) = pathA + pathB
  where
    pathsBetween' = pathsBetween graph

    svr = key M.! "svr"
    dac = key M.! "dac"
    fft = key M.! "fft"
    out = key M.! "out"

    pathA = pathsBetween' svr dac * pathsBetween' dac fft * pathsBetween' fft out
    pathB = pathsBetween' svr fft * pathsBetween' fft dac * pathsBetween' dac out

dayEleven :: IO ()
dayEleven = do
    input <- readInput
    putStrLn $ "Part One answer: " ++ show (partOne input)
    putStrLn $ "Part Two answer: " ++ show (partTwo input)

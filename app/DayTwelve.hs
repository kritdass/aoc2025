module DayTwelve where

import Data.List.Split (splitOn)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

data Region = Region {dims :: (Int, Int), quantities :: Vector Int}

readInput :: IO (Vector [Bool], [Region])
readInput = do
    contents <- readFile "inputs/DayTwelve.txt"
    let (shapes, regions) = liftA2 (,) init last $ splitOn "\n\n" contents
    pure (V.fromList (processShape <$> shapes), processRegion <$> lines regions)
  where
    processShape :: String -> [Bool]
    processShape shape = map (== '#') . concat $ (tail . lines) shape

    processRegion :: String -> Region
    processRegion region =
        Region
            { dims = (x, y)
            , quantities = V.fromList $ read <$> words quantitiesStr
            }
      where
        [dimsStr, quantitiesStr] = splitOn ": " region
        [x, y] = read <$> splitOn "x" dimsStr

partOne' :: Vector [Bool] -> Region -> Bool
partOne' shapes (Region{dims = (x, y), quantities = qtys}) = x * y >= presentArea
  where
    presentArea = V.sum $ V.imap (\i q -> q * (length . filter id) (shapes ! i)) qtys

partOne :: (Vector [Bool], [Region]) -> Int
partOne (shapes, regions) = length . filter id $ partOne' shapes <$> regions

partTwo :: (Vector [Bool], [Region]) -> String
partTwo = const "⭐"

dayTwelve :: IO ()
dayTwelve = do
    input <- readInput
    putStrLn $ "Part One answer: " ++ show (partOne input)
    putStrLn $ "Part Two answer: " ++ partTwo input

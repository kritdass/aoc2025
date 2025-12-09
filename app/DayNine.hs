module DayNine where

import Data.List (sortOn)
import Data.List.Split (splitOn)

type Point = (Int, Int)
type Segment = (Point, Point)
type Rectangle = (Point, Point)

readInput :: IO [Point]
readInput = do
    contents <- readFile "inputs/DayNine.txt"
    pure $ toPair . map read . splitOn "," <$> lines contents
  where
    toPair :: [Int] -> Point
    toPair [a, b] = (a, b)
    toPair _ = error "Bad format"

partOne :: [Point] -> Int
partOne xs =
    maximum
        [ (abs (x - x') + 1) * (abs (y - y') + 1)
        | (x, y) <- xs
        , (x', y') <- xs
        ]

segmentInRect :: Segment -> Rectangle -> Bool
segmentInRect ((x1, y1), (x2, y2)) ((a1, b1), (a2, b2)) = strictlyInside || cutsThrough
  where
    (minX, maxX) = (min a1 a2, max a1 a2)
    (minY, maxY) = (min b1 b2, max b1 b2)

    startInside = minX < x1 && x1 < maxX && minY < y1 && y1 < maxY
    endInside = minX < x2 && x2 < maxX && minY < y2 && y2 < maxY
    strictlyInside = startInside || endInside

    cutsThrough
        | x1 == x2 =
            minX < x1
                && x1 < maxX
                && min y1 y2 <= minY
                && max y1 y2 >= maxY
        | y1 == y2 =
            minY < y1
                && y1 < maxY
                && min x1 x2 <= minX
                && max x1 x2 >= maxX
        | otherwise = False

partTwo :: [Point] -> Int
partTwo xs =
    fst
        . head
        . filter
            (\(_, (p1, p2)) -> not (any (`segmentInRect` (p1, p2)) segments) && inside segments (center (p1, p2)))
        . sortOn (negate . fst)
        $ [ ((abs (x - x') + 1) * (abs (y - y') + 1), (p1, p2))
          | p1@(x, y) <- xs
          , p2@(x', y') <- xs
          ]
  where
    segments :: [Segment]
    segments = zip xs (tail xs ++ [head xs])

    center :: Rectangle -> (Double, Double)
    center ((x1, y1), (x2, y2)) = (fromIntegral (x1 + x2) / 2, fromIntegral (y1 + y2) / 2)

    rayIntersects :: (Double, Double) -> Segment -> Bool
    rayIntersects (a, b) ((x, y), (x', y')) =
        x == x'
            && fromIntegral (min y y') <= b
            && b <= fromIntegral (max y y')
            && a < fromIntegral x

    inside :: [Segment] -> (Double, Double) -> Bool
    inside segments' (x, y) = odd . length . filter (rayIntersects (x, y)) $ segments'

dayNine :: IO ()
dayNine = do
    input <- readInput
    putStrLn $ "Part One answer: " ++ show (partOne input)
    putStrLn $ "Part Two answer: " ++ show (partTwo input)

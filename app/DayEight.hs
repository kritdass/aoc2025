module DayEight where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

readInput :: IO (Vector (Int, Int, Int))
readInput = do
    contents <- readFile "inputs/DayEight.txt"
    pure . V.fromList $ toTuple . map read . splitOn "," <$> lines contents
  where
    toTuple :: [Int] -> (Int, Int, Int)
    toTuple [a, b, c] = (a, b, c)
    toTuple _ = error "Bad format"

data Distance = Distance {distance :: Int, x :: Int, y :: Int}

instance Eq Distance where
    Distance{distance = d} == Distance{distance = d'} = d == d'

instance Ord Distance where
    Distance{distance = d} <= Distance{distance = d'} = d <= d'

distances :: Vector (Int, Int, Int) -> [Distance]
distances xs =
    sort
        [ Distance{distance = dist (xs ! i) (xs ! j), x = i, y = j}
        | i <- [0 .. (V.length xs - 1)]
        , j <- [(i + 1) .. (V.length xs - 1)]
        ]
  where
    dist :: (Int, Int, Int) -> (Int, Int, Int) -> Int
    dist (x1, y1, z1) (x2, y2, z2) = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2)

mst :: Int -> Maybe Distance -> Vector Int -> [Distance] -> (Vector Int, Maybe Distance)
mst n final dsu (d@Distance{x = a, y = b} : dists)
    | parentA == parentB = mst n final dsuCompressed dists
    | otherwise = mst (n - 1) (Just d) dsuUnion dists
  where
    rep :: Int -> Vector Int -> (Int, Vector Int)
    rep i dsu'
        | dsu' ! i < 0 = (i, dsu')
        | otherwise = rep (dsu' ! i) (newDsu // [(i, parent)])
      where
        (parent, newDsu) = rep (dsu' ! i) dsu'
    (parentA, dsuA) = rep a dsu
    (parentB, dsuCompressed) = rep b dsuA
    dsuUnion :: Vector Int
    dsuUnion =
        if dsuCompressed ! parentA < dsuCompressed ! parentB
            then
                dsuCompressed // [(parentB, parentA), (parentA, dsuCompressed ! parentA + dsuCompressed ! parentB)]
            else
                dsuCompressed // [(parentA, parentB), (parentB, dsuCompressed ! parentA + dsuCompressed ! parentB)]
mst 0 final dsu _ = (dsu, final)
mst _ final dsu [] = (dsu, final)

partOne :: Vector (Int, Int, Int) -> Int
partOne xs =
    abs . product . take 3 . sort . V.toList . fst $
        mst (V.length xs - 1) Nothing (V.replicate (V.length xs) (-1)) ((take 1000 . sort . distances) xs)

partTwo :: Vector (Int, Int, Int) -> Int
partTwo xs = case final of
    Just (Distance{x = i, y = j}) -> mulXs (xs ! i) (xs ! j)
    Nothing -> error "Bad format"
  where
    final :: Maybe Distance
    final = snd $ mst (V.length xs - 1) Nothing (V.replicate (V.length xs) (-1)) ((sort . distances) xs)
    mulXs :: (Int, Int, Int) -> (Int, Int, Int) -> Int
    mulXs (x1, _, _) (x2, _, _) = x1 * x2

dayEight :: IO ()
dayEight = do
    input <- readInput
    putStrLn $ "Part One answer: " ++ show (partOne input)
    putStrLn $ "Part Two answer: " ++ show (partTwo input)

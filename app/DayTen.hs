module DayTen where

import Data.Bits
import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.SBV
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

type Machine = (Int, Vector [Int], [Int])

readMachine :: [String] -> Machine
readMachine xs = (target, buttons, joltages)
  where
    target = foldr (\c acc -> acc * 2 + (if c == '#' then 1 else 0)) 0 ((tail . init . head) xs)
    buttons = V.fromList $ map read . splitOn "," . tail . init <$> (tail . init) xs
    joltages = read <$> splitOn "," ((tail . init . last) xs)

readInput :: IO [Machine]
readInput = do
    contents <- readFile "inputs/DayTen.txt"
    let machines = words <$> lines contents
    pure $ readMachine <$> machines

partOne' :: Machine -> Int
partOne' (target, buttons, _) = popCount . head . filter (\s -> click s == target) $ sortOn popCount [0 .. combos]
  where
    combos :: Int
    combos = 2 ^ V.length buttons - 1
    click :: Int -> Int
    click spec = V.foldr xor 0 $ V.ifilter (\i _ -> testBit spec i) (foldr1 (.|.) . map bit <$> buttons)

partOne :: [Machine] -> Int
partOne = sum . map partOne'

partTwo' :: Machine -> IO Int
partTwo' (_, buttons, joltages) = do
    result <- optimize Lexicographic $ do
        presses <- sIntegers ["x" ++ show i | i <- [1 .. V.length buttons]]
        mapM_ (\press -> constrain (press .>= 0)) presses
        sequence_
            [ constrain $ counterSum r presses .== literal (fromIntegral target)
            | (r, target) <- zip [0 ..] joltages
            ]
        minimize "total_presses" $ sum presses
    case result of
        LexicographicResult model ->
            case getModelValue "total_presses" model :: Maybe Integer of
                Just v -> pure (fromIntegral v)
                Nothing -> error "Bad format"
        _ -> error "Bad format"
  where
    counterSum :: Int -> [SInteger] -> SInteger
    counterSum r pressVars =
        sum
            [ if r `elem` (buttons ! i) then p else 0
            | (i, p) <- zip [0 ..] pressVars
            ]

partTwo :: [Machine] -> IO Int
partTwo machines = do
    results <- mapM partTwo' machines
    pure $ sum results

dayTen :: IO ()
dayTen = do
    input <- readInput
    putStrLn $ "Part One answer: " ++ show (partOne input)
    partTwoAnswer <- partTwo input
    putStrLn $ "Part Two answer: " ++ show partTwoAnswer

module Main where

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as M
import DayEight (dayEight)
import DayFive (dayFive)
import DayFour (dayFour)
import DayNine (dayNine)
import DayOne (dayOne)
import DaySeven (daySeven)
import DaySix (daySix)
import DayThree (dayThree)
import DayTwo (dayTwo)
import System.Environment (getArgs)

printDay :: Int -> IO () -> IO ()
printDay day f = do
    let header = "Day " ++ show day
    putStrLn header
    putStrLn $ replicate (length header) '-'
    f
    putStrLn ""

days :: IntMap (IO ())
days =
    M.fromList . zip [1 ..] $
        [ dayOne
        , dayTwo
        , dayThree
        , dayFour
        , dayFive
        , daySix
        , daySeven
        , dayEight
        , dayNine
        ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> sequence_ $ M.mapWithKey printDay days
        [d] -> do
            let dayNum = read d
            case M.lookup dayNum days of
                Just action -> printDay dayNum action
                Nothing -> putStrLn $ "Day " ++ d ++ " not found."
        _ -> error "Too many arguments given"

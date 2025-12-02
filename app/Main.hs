module Main where

import DayOne (dayOne)
import DayTwo (dayTwo)

printDay :: Int -> IO () -> IO ()
printDay day f = do
    putStrLn $ "Day " ++ show day
    putStrLn "-----"
    f
    putStrLn ""

main :: IO ()
main = do
    printDay 1 dayOne
    printDay 2 dayTwo

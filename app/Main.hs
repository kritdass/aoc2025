module Main where

import DayOne (dayOne)

printDay :: Int -> IO () -> IO ()
printDay day f = do
    putStrLn $ "Day " ++ show day
    putStrLn "-----"
    f
    putStrLn ""

main :: IO ()
main = do
    printDay 1 dayOne

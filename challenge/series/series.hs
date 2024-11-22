module Main where

import System.Environment
    (getArgs)

import System.Exit
    (exitFailure)

series :: (Int -> Double) -> Int -> Int -> Double
series fn low high = sum $ map fn [low .. high]

geometric :: Int -> Double
geometric n = (1 / 4)^n

main :: IO()
main = do

    args <- getArgs

    if (length args) /= 2
        then do
            putStrLn "Usage: series LOW HIGH"
            exitFailure
    else do

        let 
            low = read $ head args :: Int
            high = read $ last args :: Int
            sum = series geometric low high

        putStrLn $ show sum

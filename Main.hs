module Main where

import qualified Numeric.Container as NC
import System.IO (readFile)

import KMeans (fit, cluster)

parseFromFile :: String -> [[Double]]
parseFromFile = map (map read . words) . lines

main :: IO ()
main = do
    contents <- readFile "faithful.txt"
    
    let points = map NC.fromList $ parseFromFile contents
        k      = 2

    centroids <- fit k 1e-6 points

    print centroids


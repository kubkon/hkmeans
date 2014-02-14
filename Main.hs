module Main where

import qualified Numeric.Container as NC

import qualified System.Random as R
import System.IO (readFile)

parseFromFile :: String -> [[Double]]
parseFromFile = map (map read . words) . lines

printMatrix :: NC.Matrix Double -> IO ()
printMatrix = putStr . NC.dispf 3

initialise :: Int -> NC.Matrix Double -> IO (NC.Matrix Double)
initialise k points = do
    gen <- R.newStdGen
    let m       = NC.rows points
        indices = take k $ R.randomRs (0,m-1) gen

    return $ NC.extractRows indices points

main :: IO ()
main = do
    contents <- readFile "faithful.txt"
    
    let points = NC.fromLists $ parseFromFile contents

    initialise 3 points >>= printMatrix


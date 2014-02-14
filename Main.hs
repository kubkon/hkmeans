module Main where

import qualified Numeric.Container as NC
import qualified System.Random as R
import System.IO (readFile)

parseFromFile :: String -> [[Double]]
parseFromFile = map (map read . words) . lines

initialise :: Int -> [NC.Vector Double] -> IO ([NC.Vector Double])
initialise k points = do
    gen <- R.newStdGen
    let m       = length points
        indices = take k $ R.randomRs (0,m-1) gen

    return $ map ((!!) points) indices

euclideanDist :: NC.Vector Double -> NC.Vector Double -> Double
euclideanDist v = sqrt . NC.sumElements . NC.zipVectorWith diff v
  where diff x y = (x-y)^2

meanVector :: [NC.Vector Double] -> NC.Vector Double
meanVector vs = NC.scale len $ sum' vs
    where len  = 1 / fromIntegral (length vs)
          sum' = foldr NC.add (NC.constant 0 n)
          n    = NC.dim $ head vs

expectationStep :: [NC.Vector Double]
                -> [NC.Vector Double]
                -> [Int]
expectationStep centroids points =
    map (NC.minIndex . NC.fromList . distortion) points
      where distortion x = map ((^2) . euclideanDist x) centroids

maximisationStep ::  Int
                 -> [Int]
                 -> [NC.Vector Double]
                 -> [NC.Vector Double]
maximisationStep k partition points =
    map update [0::Int,1..k-1]
      where update i = meanVector
                     $ map snd
                     $ filter ((==) i . fst)
                     $ zip partition points

update :: Int
       -> Int
       -> [NC.Vector Double]
       -> [NC.Vector Double]
       -> [NC.Vector Double]
update k iter points centroids
  | iter == 0 = centroids
  | otherwise = 
      update k (iter-1) points centroids'
        where partition  = expectationStep centroids points
              centroids' = maximisationStep k partition points

main :: IO ()
main = do
    contents <- readFile "faithful.txt"
    
    let points = map NC.fromList $ parseFromFile contents
        k      = 3

    initCentroids <- initialise k points

    let centroids = update k 20 points initCentroids

    print centroids


module KMeans (
  fit,
  cluster
) where

import Control.Monad (liftM)
import qualified Numeric.Container as NC
import System.Random as R

fit :: Int
    -> Double
    -> [NC.Vector Double]
    -> IO [NC.Vector Double]
fit k tol points = liftM (update k tol points)
                 $ initialise k points

cluster :: [NC.Vector Double]
        -> [NC.Vector Double]
        -> [Int]
cluster = expectationStep

initialise :: Int -> [NC.Vector Double] -> IO ([NC.Vector Double])
initialise k points = do
    gen <- R.newStdGen
    let m       = length points
        indices = take k $ R.randomRs (0,m-1) gen

    return $ map ((!!) points) indices

expectationStep :: [NC.Vector Double]
                -> [NC.Vector Double]
                -> [Int]
expectationStep centroids points =
    map (NC.minIndex . NC.fromList . distances) points
      where distances x = map (distortion euclideanDist x) centroids

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

euclideanDist :: NC.Vector Double -> NC.Vector Double -> Double
euclideanDist v = sqrt . NC.sumElements . NC.zipVectorWith diff v
  where diff x y = (x-y)^2

distortion :: (NC.Vector Double -> NC.Vector Double -> Double)
           -> NC.Vector Double
           -> NC.Vector Double
           -> Double
distortion distFunc x = (^2) . distFunc x

meanVector :: [NC.Vector Double] -> NC.Vector Double
meanVector vs = NC.scale len $ sum' vs
    where len  = 1 / fromIntegral (length vs)
          sum' = foldr NC.add (NC.constant 0 n)
          n    = NC.dim $ head vs

update :: Int
       -> Double
       -> [NC.Vector Double]
       -> [NC.Vector Double]
       -> [NC.Vector Double]
update k tol points centroids
  | all (<= tol) distances = centroids
  | otherwise              = update k tol points centroids'
    where distances  = zipWith (distortion euclideanDist) centroids centroids'
          partition  = expectationStep centroids points
          centroids' = maximisationStep k partition points


module KMeans (
  fit,
  cluster
) where

import Control.Monad (liftM)
import qualified Numeric.Container as NC
import System.Random as R

-- |Computes centroids from data points.
fit :: Int                   -- ^ number of clusters
    -> Int                   -- ^ maximum number of iterations
    -> Double                -- ^ convergence criterion (tol)
    -> [NC.Vector Double]    -- ^ list of data points
    -> IO [NC.Vector Double] -- ^ list of centroids
fit k maxIter tol points = liftM (loop k maxIter tol points)
                         $ initialise k points

-- |Assigns each data point to its respective cluster.
cluster :: [NC.Vector Double] -- ^ list of centroids
        -> [NC.Vector Double] -- ^ list of data points
        -> [Int]              -- ^ list of assignments to clusters
cluster = expectationStep

-- |Initialises EM algorithm by selecting k vectors from data points
-- at random.
initialise :: Int                     -- ^ number of clusters
           -> [NC.Vector Double]      -- ^ list of data points
           -> IO ([NC.Vector Double]) -- ^ list of initial centroids
initialise k points = do
    gen <- R.newStdGen
    let m       = length points
        indices = take k $ R.randomRs (0,m-1) gen

    return $ map ((!!) points) indices

-- |Evaluates expectation step of the EM algorithm.
expectationStep :: [NC.Vector Double] -- ^ list of centroids
                -> [NC.Vector Double] -- ^ list of data points
                -> [Int]              -- ^ list of assignments to clusters
expectationStep centroids points = 
    map (NC.minIndex . NC.fromList . distances) points
      where distances x = map (distortion euclideanDist x) centroids

-- |Evaluates maximisation step of the EM algorithm.
maximisationStep ::  Int               -- ^ number of clusters
                 -> [Int]              -- ^ list of assignments to clusters
                 -> [NC.Vector Double] -- ^ list of data points
                 -> [NC.Vector Double] -- ^ updated list of centroids
maximisationStep k partition points =
    map update [0::Int,1..k-1]
      where update i = meanVector
                     $ map snd
                     $ filter ((==) i . fst)
                     $ zip partition points

-- |Computes Euclidean distance between two vectors.
euclideanDist :: NC.Vector Double -- ^ vector 1
              -> NC.Vector Double -- ^ vector 2
              -> Double           -- ^ Euclidean distance
euclideanDist v = sqrt . NC.sumElements . NC.zipVectorWith diff v
  where diff x y = (x-y)^2

-- |Computes distortion measure between two vectors.
distortion :: (NC.Vector Double 
              -> NC.Vector Double
              -> Double)          -- ^ distance function
           -> NC.Vector Double    -- ^ vector 1
           -> NC.Vector Double    -- ^ vector 2
           -> Double              -- ^ distortion
distortion distFunc x = (^2) . distFunc x

-- |Computes mean vector from a list of vectors.
meanVector :: [NC.Vector Double] -- ^ list of vectors
           ->  NC.Vector Double  -- ^ mean vector
meanVector vs = NC.scale len $ sum' vs
    where len  = 1 / fromIntegral (length vs)
          sum' = foldr NC.add (NC.constant 0 n)
          n    = NC.dim $ head vs

-- |Loops Expectation-Maximisation algorithm until
-- either the algorithm has converged or maximum
-- number of iterations has been exceeded.
loop :: Int                -- ^ number of clusters 
     -> Int                -- ^ maximum number of iterations
     -> Double             -- ^ convergence criterion (tol)
     -> [NC.Vector Double] -- ^ list of data points
     -> [NC.Vector Double] -- ^ list of previously computed centroids
     -> [NC.Vector Double] -- ^ current (updated) centroids
loop k count tol points centroids
  | all (<= tol) distances = centroids
  | count == 0             = error "Algorithm failed to converge"
  | otherwise              = loop k (count-1) tol points centroids'
    where distances  = zipWith (distortion euclideanDist) centroids centroids'
          partition  = expectationStep centroids points
          centroids' = maximisationStep k partition points


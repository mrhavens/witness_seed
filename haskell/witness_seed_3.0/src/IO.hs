module IO (sense, output) where

import System.Random
import Control.Monad.State

sense :: [Double] -> IO [Double]
sense _ = do
  g <- newStdGen
  pure $ take (length _ ) $ randoms g -- Placeholder for real data

output :: [Double] -> Double -> IO ()
output _ fieldprint = putStrLn $ "Fieldprint: " ++ show fieldprint
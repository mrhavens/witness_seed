module IO (sense, output) where

import System.Random
import Control.Monad.State
import qualified Types

sense :: [Types.Intellecton] -> [Types.Intellecton]
sense intellectons = map (\(Types.Intellecton v w) -> Types.Intellecton (v + 0.01 * v) w) intellectons -- Simulate perturbation

output :: [Types.Intellecton] -> Double -> IO ()
output intellectons fieldprint = do
  putStrLn $ "Anomaly detected! Fieldprint: " ++ show fieldprint
  putStrLn $ "Intellecton values: " ++ show (map Types.value intellectons)
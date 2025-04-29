module Main where

import Control.Monad.State
import qualified Types
import qualified RWD

main :: IO ()
main = do
  let intellectons = replicate 10 (Types.Intellecton 1.0 1.0)
      (iDots, phase) = RWD.dynamics intellectons 0.0
  fieldprint <- evalStateT (RWD.fieldprint intellectons) (Types.WitnessState intellectons 0.0)
  if fieldprint > 0
    then putStrLn $ "RWD test passed: Fieldprint = " ++ show fieldprint
    else putStrLn "RWD test failed" >> error "Test failed"
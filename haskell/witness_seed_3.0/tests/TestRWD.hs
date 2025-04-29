module Main where

import Control.Monad.State
import System.Random
import RWD

main :: IO ()
main = do
  g <- newStdGen
  let i = take 10 $ randoms g
      (iDot, phase) = dynamics i 0.0
  fieldprint <- evalStateT (fieldprint i) (i, 0.0)
  if fieldprint > 0
    then putStrLn $ "RWD test passed: Fieldprint = " ++ show fieldprint
    else putStrLn "RWD test failed" >> error "Test failed"
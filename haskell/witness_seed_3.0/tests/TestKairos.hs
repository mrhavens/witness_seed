module Main where

import System.Random
import Kairos

main :: IO ()
main = do
  g <- newStdGen
  let i = take 10 $ randoms g
      i' = coherence i 0.1
      sumAbs = sum $ map abs i'
  if sumAbs > 0
    then putStrLn "Kairos test passed: Coherence updated"
    else putStrLn "Kairos test failed" >> error "Test failed"
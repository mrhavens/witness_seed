module Main where

import qualified Types
import qualified Kairos

main :: IO ()
main = do
  let intellectons = replicate 10 (Types.Intellecton 1.0 1.0)
      intellectons' = Kairos.coherence intellectons 0.1
      sumAbs = sum $ map (abs . Types.value) intellectons'
  if sumAbs > 0
    then putStrLn "Kairos test passed: Coherence updated"
    else putStrLn "Kairos test failed" >> error "Test failed"
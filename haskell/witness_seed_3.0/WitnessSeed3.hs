module Main where

import Control.Monad.State
import System.Random
import Data.Aeson (eitherDecodeFileStrict)
import qualified Data.ByteString.Lazy as BL
import qualified Types
import qualified RWD
import qualified Kairos
import qualified IO

-- Configuration
numSteps :: Int
numSteps = 1000000

dt :: Double
dt = 0.01

tauC :: Double
tauC = 1e-9

-- Main witness cycle
main :: IO ()
main = do
  -- Load sample transaction data
  eitherData <- eitherDecodeFileStrict "data/sample_transactions.json" :: IO (Either String [Types.Transaction])
  case eitherData of
    Left err -> putStrLn $ "Error loading data: " ++ err
    Right transactions -> do
      let initialIntellectons = Types.transactionsToIntellectons transactions
          initialState = Types.WitnessState initialIntellectons 0.0
      finalState <- execStateT (replicateM_ numSteps witnessCycle) initialState
      putStrLn "Witness Seed 3.0 completed."

witnessCycle :: StateT Types.WitnessState IO ()
witnessCycle = do
  state@(Types.WitnessState intellectons phase) <- get
  let intellectons' = IO.sense intellectons
      (intellectonDots, phase') = RWD.dynamics intellectons' phase
      intellectons'' = zipWith (Types.updateIntellecton dt) intellectons' intellectonDots
  fieldprint <- RWD.fieldprint intellectons''
  let intellectons''' = if fieldprint > tauC then Kairos.coherence intellectons'' phase' else intellectons''
  put $ Types.WitnessState intellectons''' phase'
  when (fieldprint > tauC) $ liftIO $ IO.output intellectons''' fieldprint
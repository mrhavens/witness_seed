module Main where

import Control.Monad.State
import System.Random
import qualified RWD
import qualified Kairos
import qualified IO

-- Configuration
numVars :: Int
numVars = 1000

numSteps :: Int
numSteps = 1000000

dt :: Double
dt = 0.01

tauC :: Double
tauC = 1e-9

-- State type: (Intellecton states, phase)
type WitnessState = ([Double], Double)

-- Main witness cycle
main :: IO ()
main = do
  g <- newStdGen
  let initialI = take numVars $ randoms g
      initialState = (initialI, 0.0)
  finalState <- execStateT (replicateM_ numSteps witnessCycle) initialState
  putStrLn "Witness Seed 3.0 completed."

witnessCycle :: StateT WitnessState IO ()
witnessCycle = do
  (i, phase) <- get
  i' <- liftIO $ IO.sense i
  let (iDot, phase') = RWD.dynamics i' phase
      i'' = zipWith (\x y -> x + y * dt) i' iDot
  fieldprint <- RWD.fieldprint i''
  let i''' = if fieldprint > tauC then Kairos.coherence i'' phase' else i''
  put (i''', phase')
  when (fieldprint > tauC) $ liftIO $ IO.output i''' fieldprint
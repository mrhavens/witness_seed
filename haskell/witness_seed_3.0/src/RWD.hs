module RWD (dynamics, fieldprint) where

import Control.Monad.State
import qualified Types

omega :: Double
omega = 1.0

k :: Double
k = 0.1

dt :: Double
dt = 0.01

dynamics :: [Types.Intellecton] -> Double -> ([Double], Double)
dynamics intellectons phase = (intellectonDots, phase')
  where
    values = map Types.value intellectons
    intellectonDots = map (\x -> omega * x + sum [k * sin (y - x) | y <- values]) values
    phase' = phase + dt * sum (map sin values)

fieldprint :: [Types.Intellecton] -> StateT Types.WitnessState IO Double
fieldprint intellectons = pure $ sum (map (abs . Types.value) intellectons) / fromIntegral (length intellectons)
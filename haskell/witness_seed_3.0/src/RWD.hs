module RWD (dynamics, fieldprint) where

import Data.List (foldl')

omega :: Double
omega = 1.0

k :: Double
k = 0.1

dt :: Double
dt = 0.01

dynamics :: [Double] -> Double -> ([Double], Double)
dynamics i phase = (iDot, phase')
  where
    iDot = map (\x -> omega * x + sum [k * sin (y - x) | y <- i]) i
    phase' = phase + dt * sum (map sin i)

fieldprint :: [Double] -> StateT ([Double], Double) IO Double
fieldprint i = pure $ sum (map abs i) / fromIntegral (length i)
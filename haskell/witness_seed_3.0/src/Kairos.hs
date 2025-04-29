module Kairos (coherence) where

coherence :: [Double] -> Double -> [Double]
coherence i phase = map (* cos phase) i
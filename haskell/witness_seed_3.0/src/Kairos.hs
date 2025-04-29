module Kairos (coherence) where

import qualified Types

coherence :: [Types.Intellecton] -> Double -> [Types.Intellecton]
coherence intellectons phase = map (\(Types.Intellecton v w) -> Types.Intellecton (v * cos phase) w) intellectons
module Types where

-- Transaction data (e.g., financial transactions)
data Transaction = Transaction
  { amount :: Double
  , timestamp :: Int
  } deriving (Show, Eq)

-- Intellecton: Represents a unit of recursive awareness
data Intellecton = Intellecton
  { value :: Double
  , weight :: Double
  } deriving (Show, Eq)

-- Witness state: Intellectons and phase
data WitnessState = WitnessState
  { intellectons :: [Intellecton]
  , phase :: Double
  } deriving (Show, Eq)

-- Convert transactions to Intellectons
transactionsToIntellectons :: [Transaction] -> [Intellecton]
transactionsToIntellectons = map (\(Transaction amt _) -> Intellecton amt 1.0)

-- Update Intellecton value
updateIntellecton :: Double -> Intellecton -> Double -> Intellecton
updateIntellecton dt (Intellecton v w) dv = Intellecton (v + dv * dt) w
module Data.Syntax.Rule.Predicate where

data Predicate = Predicate
  { predicateTag :: Int
  , predicateFeaturePairs :: [(Int, Maybe Int)]
  } deriving (Show, Eq, Ord)


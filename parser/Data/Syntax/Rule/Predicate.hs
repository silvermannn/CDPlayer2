module Data.Syntax.Rule.Predicate where

import Data.Syntax.Tag

data Predicate = Predicate
  { predicateTag :: Int
  , predicateFeaturePairs :: [(Int, Maybe Int)]
  } deriving (Show, Eq, Ord)

predicateFilter :: Predicate -> p -> TaggedWord -> Bool
predicateFilter (Predicate tagId fs) _ (TaggedWord _ _ tagId' fs') =
  tagId == tagId' && all (`checkFP` fs') fs

checkFP :: (Eq a1, Eq a2) => (a1, Maybe a2) -> [(a1, a2)] -> Bool
checkFP fp = any (fit fp)
  where
    fit (fn1, Nothing) (fn2, _) = fn1 == fn2
    fit (fn1, Just fv1) (fn2, fv2) = fn1 == fn2 && fv1 == fv2

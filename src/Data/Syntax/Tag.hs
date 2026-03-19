module Data.Syntax.Tag where

data TaggedWord = TaggedWord
  { index :: Int
  , wordId :: Int
  , tagId :: Int
  , features :: [(Int, Int)]
  } deriving (Show, Eq, Ord)

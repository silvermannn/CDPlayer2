module Data.Syntax.Tag where

data TaggedWord = SWord
  { index :: Int
  , wordId :: Int
  , tagId :: Int
  , features :: [(Int, Int)]
  } deriving (Show)

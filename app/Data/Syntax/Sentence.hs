module Data.Syntax.Sentence where

data TaggedWord = SWord
  { index :: Int
  , wordId :: Int
  , tagId :: Int
  , features :: [(Int, Int)]
  , used :: Bool
  } deriving (Show)

newtype Sentence =
  Sentence [TaggedWord]
  deriving (Show)

filterBy :: (TaggedWord -> Bool) -> Sentence -> [TaggedWord]
filterBy p (Sentence ws) = filter p $ filter (not . used) ws

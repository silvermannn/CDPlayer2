module Data.Syntax.Tag where

data TaggedWord = TaggedWord
  { index :: Int
  , wordId :: Int
  , tagId :: Int
  , features :: [(Int, Int)]
  } deriving (Show)

instance Eq TaggedWord where
  (==) (TaggedWord _ _ t1 fs1) (TaggedWord _ _ t2 fs2) = t1 == t2 && fs1 == fs2

instance Ord TaggedWord where
  compare (TaggedWord _ _ t1 fs1) (TaggedWord _ _ t2 fs2) =
    if c1 == EQ
      then compare fs1 fs2
      else c1
    where
      c1 = compare t1 t2

module Data.Syntax.Sentence where

import Data.Syntax.Tag

newtype Sentence =
  Sentence [TaggedWord]
  deriving (Show)

filterBy :: (TaggedWord -> Bool) -> Sentence -> [TaggedWord]
filterBy p (Sentence ws) = filter p ws

removeUsed :: TaggedWord -> Sentence -> Sentence
removeUsed tw (Sentence tws) = Sentence $ filter ((/= index tw) . index) tws

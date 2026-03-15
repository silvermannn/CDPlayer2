module Data.Syntax.Sentence where

import qualified Data.IntMap as I
import Data.Syntax.Tag

type Sentence = I.IntMap TaggedWord

filterBy :: (TaggedWord -> Bool) -> Sentence -> [TaggedWord]
filterBy p m = I.elems $ I.filter p m

removeUsed :: TaggedWord -> Sentence -> Sentence
removeUsed tw m = I.delete (index tw) m

sentenceSize s = I.size s

isEmptySentence :: Sentence -> Bool
isEmptySentence m = I.null m

newSentence :: [TaggedWord] -> Sentence
newSentence = I.fromList . map (\t@(SWord i _ _ _) -> (i, t))

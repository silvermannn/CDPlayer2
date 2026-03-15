module Data.Syntax.Rule.Evolution where

import Control.Monad
import Control.Monad.Extra

import Data.Function
import Data.List

import Data.Random.Stateful
import Data.Syntax.DependencyTree
import Data.Syntax.Rule
import Data.Syntax.Rule.Application
import Data.Syntax.Rule.Evaluation
import Data.Syntax.Rule.Random
import Data.Syntax.Sentence

data EvolutionParameters = EvolutionParameters
  { maxPopulationSize :: Int
  , maxRulesetSize :: Int
  , mutationRate :: Float
  , survivalRate :: Float
  , generationParams :: RuleGenerationParams
  } deriving (Show)

newtype Population =
  Population [RuleSet]

generateInitialPopulation :: EvolutionParameters -> StatefulRandom Population
generateInitialPopulation p =
  Population
    <$> replicateM (maxPopulationSize p) (generateRuleSet (generationParams p) (maxRulesetSize p))

makeMutations :: EvolutionParameters -> Population -> StatefulRandom Population
makeMutations p (Population rs) = do
  rs' <- mapM (makeMutation p) rs
  return $ Population rs'

mutationMultiplier :: Int
mutationMultiplier = 10000

makeMutation :: EvolutionParameters -> RuleSet -> StatefulRandom RuleSet
makeMutation p rs = do
  n <- generateRandomMax mutationMultiplier
  if n > truncate (fromIntegral mutationMultiplier * mutationRate p)
    then return rs
    else mutateRuleSet (generationParams p) rs

makeCrossovers :: EvolutionParameters -> Population -> StatefulRandom Population
makeCrossovers p (Population rs) = do
  srs <- shuffle rs
  rs' <- zipWithM crossover2RuleSets (take m $ cycle rs) (cycle srs)
  return $ Population (rs ++ rs')
  where
    m = maxPopulationSize p - length rs

cutPopulation ::
     EvolutionParameters -> Population -> DependencyTree -> Sentence -> StatefulRandom Population
cutPopulation p (Population rs) dt s = return $ Population survived
  where
    ress = map (`parseSentence` s) rs
    scores = zipWith (evaluateResults dt) ress rs
    survived = take survivedN $ map fst $ sortBy (compare `on` snd) $ zip rs scores
    survivedN = ceiling (fromIntegral (length rs) * survivalRate p)

evolutionStep ::
     EvolutionParameters -> DependencyTree -> Sentence -> Population -> StatefulRandom Population
evolutionStep p dt s ps = do
  survived <- cutPopulation p ps dt s
  crossed <- makeCrossovers p survived
  makeMutations p crossed

infiniteEvolution ::
     EvolutionParameters -> DependencyTree -> Sentence -> Population -> StatefulRandom [Population]
infiniteEvolution p dt s = iterateM (evolutionStep p dt s)

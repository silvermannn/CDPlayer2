module Data.Syntax.Rule.Evolution where

import Control.Monad
import Control.Monad.State

import Data.Random.Stateful
import Data.Syntax.Rule
import Data.Syntax.Rule.Application
import Data.Syntax.Rule.Evaluation
import Data.Syntax.Rule.Random

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
makeMutations = undefined

makeCrossovers :: EvolutionParameters -> Population -> Population -> StatefulRandom Population
makeCrossovers = undefined

cutPopulation :: EvolutionParameters -> Population -> StatefulRandom Population
cutPopulation = undefined

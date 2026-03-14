module Data.Syntax.Rule where

import GHC.Generics

import qualified Data.Map as M

import System.Random

import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Sentence
import Data.Syntax.Tag
import Data.Syntax.Rule.Predicate
import Data.TreeSearch

data SearchDirection
  = SearchLeft
  | SearchRight
  deriving (Show, Eq, Generic, Finite, Uniform)

data Rule
  = FindRoot Predicate
  | FindLink Predicate Predicate SearchDirection Int DependencyRelation
  deriving (Show, Eq)

newtype RuleSet =
  RuleSet [Rule]
  deriving (Show, Eq)

module Data.Random.Stateful where

import System.Random

import Control.Monad.State

type StatefulRandom a = State StdGen a

generateRandom :: Uniform a => StatefulRandom a
generateRandom = do
  gen <- get
  let (r, gen') = uniform gen
  put gen'
  return r

generateRandomMax :: (Enum a, UniformRange a) => a -> StatefulRandom a
generateRandomMax m = do
  gen <- get
  let (r, gen') = uniformR (toEnum 0, m) gen
  put gen'
  return r

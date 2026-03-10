module Main (main) where

import Data.Syntax.DependencyTree

tree :: DependencyTree () ()
tree = emptyTree

main :: IO ()
main = print tree

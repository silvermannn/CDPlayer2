module Main (main) where

import Data.Syntax.DependencyTree

tree :: DependencyTree () ()
tree = singleton () () 

main :: IO ()
main = showDependencyTree tree

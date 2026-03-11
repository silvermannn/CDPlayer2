{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module Data.Syntax.DependencyTree where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor
import Data.Tree
import GHC.Generics

data DependencyTree a b = DependencyTree
  { node :: a
  , relation :: b
  , children :: [DependencyTree a b]
  } deriving (Show, Generic, ToJSON, FromJSON, Functor)

instance Bifunctor DependencyTree where
  bimap f g (DependencyTree n r chs) =
    DependencyTree (f n) (g r) (bimap f g <$> chs)

singleton :: a -> b -> DependencyTree a b
singleton a r = DependencyTree a r []

toTree :: DependencyTree a b -> Tree (b, a)
toTree (DependencyTree n r chs) = Node (r, n) $ map toTree chs

showDependencyTree :: (Show a, Show b) => DependencyTree a b -> IO ()
showDependencyTree dt = putStrLn $ drawTree $ fmap show $ toTree dt

splits :: [a] -> [([a], a, [a])]
splits [] = []
splits (a:as) = scanl (\(xs, x, (y:ys)) _ -> (x : xs, y, ys)) ([], a, as) as

searchAndModifyNode ::
     (a -> Bool)
  -> (DependencyTree a b -> DependencyTree a b)
  -> DependencyTree a b
  -> [DependencyTree a b]
searchAndModifyNode p m dt@(DependencyTree n r chs) =
  [m dt | p n]
    ++ [ DependencyTree n r (pchs ++ (ch' : achs))
       | (pchs, ch, achs) <- splits chs
       , ch' <- searchAndModifyNode p m ch
       ]

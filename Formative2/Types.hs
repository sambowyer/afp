module Types where

data Tree a b = Leaf a | Node b [Tree a b]
     deriving (Show,Eq)

data Op = Add | Mul  deriving (Show,Eq)
type OpTree a = Tree a Op

class Monad m => PickingMonad m where
   pick :: Int -> Int -> m Int


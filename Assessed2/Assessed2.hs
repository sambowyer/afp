-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Assessed2 (choose , simulate , cut , shuffle , riffles , permute , genTree) where

import Types
import Data.List

standard52 :: Deck
standard52 = [Card {rank = r, suit = s} | r <- [R2 .. RA], s <- [C .. S]]

code :: PickingMonad m => m Char
code = do
  i <- pick 0 3
  return ("hello" !! i)

prob :: Eq a => Dist a -> a -> Rational
prob xm x = sum [p | (y,p) <- dist xm, x == y]

normalise :: Eq a => Dist a -> Dist a
normalise xm = Dist [(x,prob xm x) | x <- support xm]
  where
    support :: Eq a => Dist a -> [a]
    support xm = nub [x | (x,p) <- dist xm, p > 0]  -- "nub" removes duplicates from a list

instance Show Rank where
  show R2 = "2"
  show R3 = "3"
  show R4 = "4"
  show R5 = "5"
  show R6 = "6"
  show R7 = "7"
  show R8 = "8"
  show R9 = "9"
  show R10 = "10"
  show RJ = "J"
  show RQ = "Q"
  show RK = "K"
  show RA = "A"

instance Show Suit where
  show C = "\9827"
  show D = "\9830"
  show H = "\9829"
  show S = "\9824"

red, black :: String -> String
red   s = "\x1b[31m" ++ s ++ "\x1b[0m"
black s = "\x1b[30m" ++ s ++ "\x1b[0m"

instance Show Card where
  show (Card r s) | s == D || s == H = red ((show r) ++ (show s))
                  | otherwise        = black ((show r) ++ (show s))

choose :: PickingMonad m => [a] -> m a
choose [] = undefined
choose xs = do
    i <- pick 0 (length xs - 1)
    return (xs !! i)

simulate :: Monad m => m Bool -> Integer -> m Integer
simulate bm 0 = return (0)
simulate bm n = do
                i <- bm
                if i == True then  return (+1) <*> simulate bm (n-1)
                else simulate bm (n-1)

cut :: PickingMonad m => [a] -> m ([a],[a])
cut [] = return ([], [])
cut xs = do
         i <- pick 0 (length xs)
         return (take i xs, drop i xs)

shuffle :: PickingMonad m => ([a],[a]) -> m [a]
shuffle ([], ys) = return ys
shuffle (xs, []) = return xs
shuffle (x:xs, y:ys) = do
                       i <- pick 1 ((length (x:xs)) + (length (y:ys)))
                       if i <= length (x:xs) then return (:) <*> return x <*> shuffle (xs, y:ys)
                       else return (:) <*> return y <*> shuffle (x:xs, ys)

riffles :: PickingMonad m => ([a] -> m ([a],[a])) -> (([a],[a]) -> m [a]) -> Int -> [a] -> m [a]
riffles _  _  0 xs = return xs
riffles cf sf n xs = do
                    ys <- (cf xs) >>= sf
                    riffles cf sf (n-1) ys

permute :: PickingMonad m => [a] -> m [a]
permute [] = return []
permute (x:xs) = do
               pxs <- permute xs
               (ys, zs) <- cut pxs
               return (concat [ys, (x:zs)])


genTree :: PickingMonad m => [a] -> m (Bin a)
genTree []  = undefined
genTree [x] = return (L x)
genTree (x:xs) = do
                 tx <- genTree xs
                 i <- pick 0 (2*(length xs) - 2)
                 (tx', n) <- replace i x tx
                 return (tx') where
                     replace :: PickingMonad m => Int -> a -> Bin a -> m ((Bin a), Int)
                     replace 0 y t = do
                         l <- choose [True, False]
                         if l == True then return (B (L y) t, 0)
                         else return (B t (L y), 0)
                     replace n y (L z) = return ((L z), (n))
                     replace n y (B ta tb) = do
                         (t, n') <- replace (n-1) y ta
                         if n' <= 0 then return ((B t tb), n')
                         else do
                             (t', n'') <- replace (n'-1) y tb
                             return ((B ta t'), n'')

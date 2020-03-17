-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Assessed3 where

import Data.List
import Data.Tree

import Types
import DomViz  -- comment out as a last resort if you are unable to install diagrams

-- given a cell c and a player p, compute the adjacent cell c'
-- that is also occupied if p plays a domino at c
adjCell :: Cell -> Player -> Cell
adjCell (x,y) H = (x+1,y)
adjCell (x,y) V = (x,y+1)

-- compute the opponent of a player
opp :: Player -> Player
opp H = V
opp V = H

-- determine whether a move is valid in a given board
valid :: Board -> Cell -> Bool
valid b c = c `elem` free b && adjCell c (turn b) `elem` free b

-- create an empty board from an arbitrary list of cells
empty :: [Cell] -> Board
empty cs = Board { turn = H, free = cs, hist = [] }

-- create a rectangular board of arbitrary dimensions
board :: Int -> Int -> Board
board maxx maxy = empty [(x,y) | x <- [1..maxx], y <- [1..maxy]]

-- create a crosshatch-shaped square board of arbitrary dimension
hatch :: Int -> Board
hatch n = empty [(x,y) | x <- [1..2*n+1], y <- [1..2*n+1], odd y || x == 1 || x == (2*n+1) || odd x]

-- some example Domineering games
board4x4_3 = Board { turn = H,
                     free = [(1,1),(1,2),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)],
                     hist = [(1,3),(2,1)] }

alphaDom_vs_LeeSedom =
  Board { turn = V,
          free = [(-4,1),(-4,3),(-2,0),(-2,4),(2,1),(2,4),(3,-4),(3,4),(4,-2),(4,0)],
          hist = [(0,4),(4,1),(0,-4),(-4,-3),(-1,-2),(2,-1),(-2,-4),(-4,-1),(-1,2),(4,3),(1,2),(-2,2),(-4,-4),(-2,-2),(2,-2),(4,-4),(-3,1),(2,-4),(-4,4),(-1,3),(-4,2),(-3,-2),(3,-1),(1,-3),(-2,-3),(3,1),(1,3)] }

alphaDom_vs_RanDom =
  Board { turn = V,
          free = [(-4,-3),(-4,0),(-2,-4),(-2,-2),(-1,-4),(-1,-2),(-1,2),(-1,4),(0,-4),(0,-2),(0,2),(0,4),(1,-4),(1,-2),(1,2),(1,4),(2,-4),(2,-2),(2,4),(3,-4),(4,0),(4,3)],
          hist = [(-3,4),(2,-1),(-3,2),(4,-2),(-4,-4),(-4,3),(3,4),(2,1),(-3,1),(3,1),(-4,-1),(-2,-1),(-2,3),(-4,1),(1,3),(4,-4),(-4,-2),(4,1),(1,-3),(3,-2),(-2,-3)] }

-- start of Question 1

legalMoves :: Player -> Board -> [Cell]
legalMoves p (Board _ f h) = [c | c <- f, valid (Board p f h) c]

moveLegal :: Board -> Cell -> Board
moveLegal (Board p f h) c | valid (Board p f h) c = Board (opp p) ([c' | c' <- f, c' /= c && c' /= (adjCell c p)]) ((c):h)
                          | otherwise = undefined

replay :: Board -> [Board]
replay b = concat [(replay' b), [b]] where
    replay' :: Board -> [Board]
    replay' (Board p f []) = []
    replay' (Board p f (h:hs)) = concat [(replay' b'), [b']] where
        b' = Board (opp p) (concat [f, [h, (adjCell h (opp p))]]) hs

-- start of Question 2

gametree :: Board -> Tree Board
gametree b = Node b [gametree (moveLegal b c) | c <- legalMoves (turn b) b]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

score :: Board -> Score
score (Board p f h) | legalMoves p (Board p f h) == [] = Win (opp p)
                    | otherwise = Heu ((length (legalMoves V (Board p f h))) - (length (legalMoves H (Board p f h))) - s) where
                        s = if p == V then 1 else -1

minimax :: (Board -> Score) -> Tree Board -> Tree (Board, Score)
minimax sfn (Node b []) = Node (b, sfn b) []
minimax sfn (Node b ts)
    | turn b == H = Node (b, minimum scores) ts'
    | otherwise = Node (b, maximum scores) ts' where
        ts' = map (minimax sfn) ts
        scores = [s | Node (_,s) _ <- ts']

bestmoves :: Int -> (Board -> Score) -> Board -> [Cell]
bestmoves d sfn b = [head (hist (fst (rootLabel node))) | node <- ts, snd (rootLabel node) == bestScore] where
    Node _ ts = minimax sfn (prune d (gametree b))
    scoreList = map (snd . rootLabel) ts
    bestScore = if turn b == H then minimum scoreList
                else maximum scoreList


chooseSafe :: PickingMonad m => [a] -> m (Maybe a)
chooseSafe [] = return Nothing
chooseSafe xs = do
  i <- pick 0 (length xs - 1)
  return (Just (xs !! i))

randomBestPlay :: PickingMonad m => Int -> (Board -> Score) -> Board -> m (Maybe Cell)
randomBestPlay d sfn = chooseSafe . bestmoves d sfn
randomPlay :: PickingMonad m => Board -> m (Maybe Cell)
randomPlay b = chooseSafe (legalMoves (turn b) b)

-- start of Question 3

runGame :: PickingMonad m => (Board -> m (Maybe Cell)) -> (Board -> m (Maybe Cell)) -> Board -> m Board
runGame playH playV b = do
                        mc <- if turn b == H then playH b else playV b
                        if mc == Nothing then return b else do
                            let Just c = mc
                            if c `elem` legalMoves (turn b) b then runGame playH playV (moveLegal b c) else return b
-- start of Question 4

carpets :: [Board]
carpets = [carpets' i | i <- [0..]] where
    carpets' :: Int -> Board
    carpets' 0 = Board H [(1,1)] []
    carpets' n = Board (if odd n then V else H) [(x+((3^(n-1))*a),y+((3^(n-1))*b)) | (x,y) <- free (carpets' (n-1)), a <- [1..3], b <- [1..3], (a,b) /= (2,2)] []

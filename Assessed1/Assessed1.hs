-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Assessed1 (doubleList, firstDoubled , priceRange , allergyFree , checkSpec , checkSpec' , linearSort , counterexample , fromBin , toBin) where

import Data.List
import Data.Maybe

import Types

doubleList :: [a] -> [a]
doubleList [] = []
doubleList (x:xs) = x:x:(doubleList xs)

firstDoubled :: Eq a => [a] -> Maybe a
firstDoubled [] = Nothing
firstDoubled (x:[]) = Nothing
firstDoubled (x:y:ys) | x == y = Just x
                      | otherwise = firstDoubled (y:ys)

priceRange :: Price -> Price -> [Cupcake] -> [Cupcake]
priceRange _ _ [] = []
priceRange min max ((CC p r):xs) | (p >= min) && (p <= max) = (CC p r):(priceRange min max xs)
                                 | otherwise = (priceRange min max xs)

allergyFree :: [Ingredient] -> [Cupcake] -> [Cupcake]
allergyFree _ [] = []
allergyFree xs ((CC p r):ys) | intersect xs r == [] = (CC p r):(allergyFree xs ys)
                             | otherwise = allergyFree xs ys

sampletin :: Tin
sampletin = [[Nuts], [Dairy,Gluten], [], [Soy]]

checkSpec :: Spec -> Tin -> Bool
checkSpec (And x y) t = (checkSpec x t) && (checkSpec y t)
checkSpec (Or x y)  t = (checkSpec x t) || (checkSpec y t)
checkSpec (Not x)   t = (checkSpec x t) == False
checkSpec (HasCup a b) t  = elem b (t!!a)

checkSpec' :: Spec -> Tin -> Maybe Bool
checkSpec' (And x y) t | ((checkSpec' x t) == Nothing) || ((checkSpec' y t) == Nothing) = Nothing
                       | otherwise = Just (((checkSpec' x t) == (Just True)) && ((checkSpec' y t) == (Just True)))
checkSpec' (Or x y)  t | ((checkSpec' x t) == Nothing) || ((checkSpec' y  t) == Nothing) = Nothing
                       | otherwise = Just (((checkSpec' x t) == (Just True)) || ((checkSpec' y t) == (Just True)))
checkSpec' (Not x)   t | (checkSpec' x t) == Nothing = Nothing
                       | otherwise = Just ((checkSpec' x t) == (Just False))
checkSpec' (HasCup a b) t  | (a >= (length t)) || (a < 0) = Nothing
                           | otherwise = Just (elem b (t!!a))

linearSort :: Ord a => [a] -> [a]
linearSort [] = []
linearSort xs = linearSort' xs [] where
    linearSort' :: Ord a => [a] -> [a] -> [a] --2nd argument is for stack
    linearSort' [] ys = ys
    linearSort' (x:xs) ys  | ys == []       = linearSort' xs (x:[])
                           | x <= (head ys) = linearSort' xs (x:ys)
                           | otherwise      = (head ys):(linearSort' (x:xs) (tail ys))

counterexample :: [Int]
counterexample = [2,3,1]


--fromBin MIGHT work by doing a breadth-first traversal and assigning
--each B node a number given by (no of B nodes to the left of it in the tree + 1)
fromBin :: Bin -> [Int]
fromBin L = []
fromBin (B x y) = mappend ( (1+(bCount x)):(fromBin x) ) (map (+ ((bCount x)+1 )) (fromBin y) ) where
    bCount :: Bin -> Int
    bCount L = 0
    bCount (B x y) = 1 + (bCount x) + (bCount y)

toBin :: [Int] -> Maybe Bin
toBin [] = Just L
toBin (x:xs) | (firstSmallerIndex (snd split) x) < length (snd split) = Nothing
             | otherwise = Just (B (fromJust $ (toBin (fst split))) (fromJust $ (toBin(snd split)))) where
                    split = (splitAt (firstLargerIndex xs x) xs)

firstLargerIndex :: [Int] -> Int -> Int
firstLargerIndex [] y = 0
firstLargerIndex (x:xs) y | x > y = 0
                          | otherwise = 1 + (firstLargerIndex xs y)

firstSmallerIndex :: [Int] -> Int -> Int
firstSmallerIndex [] y = 0
firstSmallerIndex (x:xs) y | x < y = 0
                           | otherwise = 1 + (firstSmallerIndex xs y)

not231general :: [Int] -> Bool
not231general [] = True
not231general (x:xs) = let k = findIndex (> x) xs in
                      if isNothing k then not231general xs else
                      let j = findIndex (< x) (take (length xs - (fromJust k) + 1) (drop (fromJust k) xs)) in
                      if isNothing j then not231general xs else False

perms :: [[Int]]
perms = [x | n <- [0..11], x <- permutations [1..n], not231general x]

test_one :: Bool
test_one = all (\x -> x) [fromBin (fromJust $ toBin xs) == xs | xs <- perms]

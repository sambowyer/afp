-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Formative3 (emptyTrie , insertTrie , trieToList , findTrie , isEmpty , epsilon , wordrects) where

import Data.Char

readDict :: String -> IO [String]
readDict path = do
  content <- readFile path
  return (map (map toUpper) $ lines content)

data Trie = T (Char, [Trie]) | Empty
    deriving (Eq, Show)
-- replace this by an appropriate type, data, or newtype definition of your choice
-- if Char = '-' then we are at the root of the full tries
-- if Char = '.' then we are at the end of a word

-- construct the empty trie
emptyTrie :: Trie
emptyTrie = Empty

-- insert a string into a trie
insertTrie :: String -> Trie -> Trie
insertTrie "" (T (c, ts)) = (T (c, ((T ('.', [])):ts)))
insertTrie [] Empty = (T ('_', [T ('.', [])]))
insertTrie (x:xs) Empty = T ('_', [insertTrie xs (T (x, []))])
insertTrie str (T (c, ts)) = T (c, insertTrieFromChar str ts) where
        insertTrieFromChar :: String -> [Trie] -> [Trie]
        insertTrieFromChar (x:xs) [] = [insertTrie xs (T (x, []))]
        insertTrieFromChar (x:xs) ((T (c, cts)):ts) | x == c = ((insertTrie xs (T (c, cts))):ts)
                                                    | otherwise = ((T (c, cts)):(insertTrieFromChar (x:xs) ts))
        insertTrieFromChar _ _ = [Empty]

-- given a trie, return the list of strings it contains
trieToList :: Trie -> [String]
trieToList Empty = []
trieToList (T ('_', ts)) = concat (map trieToList ts)
trieToList (T ('.', _)) = [""]
trieToList (T (_, [])) = []
trieToList (T (c, t:ts))   = concat ((map (c:) (trieToList t)):(map (map (c:)) (map trieToList ts)))

-- given a string xs and a trie t, return a (possibly empty) trie containing all of the possible suffixes of xs in t
findTrie :: String -> Trie -> Trie
findTrie _ Empty = Empty
findTrie [] t = t
findTrie str (T ('_', ts)) = nonEmptyFindTrie str ts
findTrie (x:xs) (T (c, ts)) | x == c = if length xs == 0 then (T ('_', ts)) else nonEmptyFindTrie xs ts
                            | otherwise = Empty

nonEmptyFindTrie :: String -> [Trie] -> Trie
nonEmptyFindTrie str ts | length ts' /= 0 = head ts'
                        | otherwise       = Empty where
                            ts' = filter (Empty /=) (map (findTrie str) ts)

-- return True if the trie is empty, or else False
isEmpty :: Trie -> Bool
isEmpty Empty = True
isEmpty _ = False

-- return True if the trie contains the empty string "", or else False
epsilon :: Trie -> Bool
epsilon (T ('_', ts)) | (T ('.', [])) `elem` ts = True
                      | otherwise = False
epsilon _ = False

wordrects :: [String] -> Int -> Int -> [String] -> [[String]]
wordrects dict rows cols crib = undefined

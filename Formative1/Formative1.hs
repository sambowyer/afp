module Formative1 (translate, isMortal , lifetime , hasPeriod , findEventualPeriod , eventuallyPeriodic) where
import Data.List

type Cell = (Int,Int)
type Grid = [Cell]

isLive, isDead :: Cell -> Grid -> Bool
isLive c g = c `elem` g
isDead c g = not (isLive c g)

neighbours :: Cell -> [Cell]
neighbours (x,y) = [ (x+i,y+j) | i <- [-1..1], j <- [-1..1], not (i==0 && j==0) ]

liveNeighbours :: Grid -> Cell -> [Cell]
liveNeighbours g c = [c' | c' <- neighbours c, isLive c' g]

step :: Grid -> Grid
step [] = []
step g =
  [(x,y) | x <- [minX-1 .. maxX+1],
           y <- [minY-1 .. maxY+1],
              (isDead (x,y) g && length (liveNeighbours g (x,y)) == 3)
           || (isLive (x,y) g && length (liveNeighbours g (x,y)) `elem` [2,3])
         ]
  where
    minX = minimum [ x | (x,y) <- g ]
    maxX = maximum [ x | (x,y) <- g ]
    minY = minimum [ y | (x,y) <- g ]
    maxY = maximum [ y | (x,y) <- g ]

pentagenarian, glider, block, pulsar, smallblock, pentadecathlon, die_hard:: Grid
pentagenarian = [(1,2),(2,2),(2,3),(4,1),(4,3)]
glider = [(1,3),(2,1),(2,3),(3,2),(3,3)]
block = [(x,y) | x <- [1..3], y <- [1..3]]
pulsar = [(x+i,y) | x <- [2,8], y <- [0,5,7,12], i <- [0..2]] ++ [(x,y+i) | x <- [0,5,7,12], y <- [2,8], i <- [0..2]]
smallblock =[(1,1),(1,2),(2,1),(2,2)]
pentadecathlon = [(1, 2), (2, 2), (3, 1), (3, 3), (4, 2), (5, 2), (6, 2), (7, 2), (8, 1), (8, 3), (9, 2), (10, 2)]
die_hard = [(1, 2), (2, 2), (2, 3), (6, 3), (7, 1), (7, 3), (8, 3)]

translate :: (Int,Int) -> Grid -> Grid
translate (m,n) g = [ (x+m, y+n) | (x,y) <- g]

isMortal :: Grid -> Bool
isMortal [] = True
isMortal g = isMortal(step g)

lifetime :: Grid -> Maybe Int
lifetime [] = Just 0
lifetime g = case (lifetime (step g)) of
    Nothing -> Nothing
    Just n -> Just (n + 1)

hasPeriod :: Grid -> Int -> Bool
hasPeriod g k = sameGrid ((iterate step g) !! k) g

sameGrid :: Grid -> Grid -> Bool
sameGrid g1 g2 = sort g1 == sort g2

eventuallyPeriodic :: Grid -> Bool
eventuallyPeriodic [] = True
eventuallyPeriodic g = checkForPeriod 2 g
  where
    checkForPeriod :: Int -> Grid -> Bool
    checkForPeriod n g = containsDuplicate (take n (iterate step g)) || checkForPeriod (n+1) g

containsDuplicate :: [Grid] -> Bool
containsDuplicate [] = False
containsDuplicate gs = elem (sort(head gs)) ([sort g | g <- (tail gs)]) || containsDuplicate (tail gs)

findEventualPeriod :: Grid -> Maybe (Int,Int)
findEventualPeriod g = Just ((originalDuplicateIndex (take (findFirstDuplicateIndex 2 g) (iterate step g)) 0), (findFirstDuplicateIndex 2 g) - (originalDuplicateIndex (take (findFirstDuplicateIndex 2 g) (iterate step g)) 0)-1)
  where
    findFirstDuplicateIndex :: Int -> Grid -> Int
    findFirstDuplicateIndex n g = if (containsDuplicate (take n (iterate step g)) == True) then n
    else findFirstDuplicateIndex (n+1) g

    originalDuplicateIndex :: [Grid] -> Int -> Int
    originalDuplicateIndex gs n =
      if (sameGrid (sort(gs !! n)) (sort(gs !! ((length gs)-1))) == True) then
        n
      else
        originalDuplicateIndex gs (n+1)

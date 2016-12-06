{-# LANGUAGE RankNTypes #-}

module Redundancy where

import Data.List
import Control.Arrow

type OccurancePredicate =
    forall a t. Foldable t => (a -> a -> Ordering) -> t a -> a

countOccurances :: Ord a => [a] -> [(a, Int)]
countOccurances =
    map (head &&& length) . group . sort

compareOccurances :: Ord a => (a, Int) -> (a, Int) -> Ordering
compareOccurances (_, numA) (_, numB) =
    compare numA numB

determineOccuranceBy :: Ord a => OccurancePredicate -> [a] -> a
determineOccuranceBy predicate =
    fst . predicate compareOccurances . countOccurances

run :: OccurancePredicate -> IO ()
run predicate = do
    redundancyMatrix <- transpose . lines <$> readFile "./input.txt"
    putStrLn $ map (determineOccuranceBy predicate) redundancyMatrix

partOne :: IO ()
partOne =
    run maximumBy

partTwo :: IO ()
partTwo =
    run minimumBy

main :: IO ()
main = do
    partOne
    partTwo

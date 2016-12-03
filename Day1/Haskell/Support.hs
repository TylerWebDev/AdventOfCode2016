module Support where

import Data.Char (isSpace)
import qualified Data.Set as Set

-- Generate a range of integers between two values; can be increasing or decreasing
generateRange :: Int -> Int -> [Int]
generateRange a b
    | b < a =
        [a, (a - 1) .. b]
    | otherwise =
        [a, (a + 1) .. b]

-- Turn a list into pairs, grouping each element with the next. [1, 2, 3] == [(1, 2), (2, 3)]
tupleifyList :: [a] -> [(a, a)]
tupleifyList [] =
    []
tupleifyList [_] =
    []
tupleifyList (x:xs) =
    (x, head xs) : tupleifyList xs

-- Finds the first duplicate item in a list, or fails
firstDuplicateIn :: Ord a => [a] -> Maybe a
firstDuplicateIn list =
    duplicate' list Set.empty
        where
            duplicate' [] _ = Nothing
            duplicate' (x:xs) set =
                if Set.member x set then
                    Just x
                else
                    duplicate' xs (Set.insert x set)

-- Calculates the distance on a grid between two vector Positions
taxicabDistance :: (Int, Int) -> (Int, Int) -> Int
taxicabDistance (p1, p2) (q1, q2) =
    abs (p1 - q1) + abs (p2 - q2)

-- Remove whitespace from beginning and end of a string
trim :: String -> String
trim =
    f . f
        where f = reverse . dropWhile isSpace

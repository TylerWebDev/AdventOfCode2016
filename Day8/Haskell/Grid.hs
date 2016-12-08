module Grid where

import Data.List
import Control.Lens

type Instruction =
    (Grid -> Grid)

type Grid =
    [[Bool]]

dimX = 50
dimY = 6

showGrid :: Grid -> String
showGrid elems =
    unlines $ (map . map) (\b -> if b then '#' else '.') elems

initialGrid :: Grid
initialGrid =
    [ [ False | _ <- [1..dimX :: Int] ] | _ <- [1..dimY :: Int] ]

rotate :: [a] -> [a]
rotate [] =
    []
rotate list =
    last list : init list

rotateN :: Int -> [a] -> [a]
rotateN n =
    last . take (n + 1) . iterate rotate

makeRectGrid :: Int -> Int -> Grid
makeRectGrid x y =
    [ rows y' | y' <- [1..dimY :: Int] ]
    where
        rows y' =
            if y' <= y then
                [ cols x' | x' <- [1..dimX :: Int] ]
            else
                [ False | _ <- [1..dimX :: Int] ]
        cols x' =
            x' <= x

combineGrid :: Grid -> Grid -> Grid
combineGrid =
    (zipWith . zipWith) (||)

rotateRow :: Int -> Int -> Instruction
rotateRow i amt =
    over (ix i) (rotateN amt)

rotateCol :: Int -> Int -> Instruction
rotateCol i amt =
    transpose . rotateRow i amt . transpose

rect :: Int -> Int -> Instruction
rect x y =
    combineGrid (makeRectGrid x y)

countOn :: Grid -> Int
countOn grid =
    sum $ map (length . filter (== True)) grid

runInstructions :: [Instruction] -> Grid
runInstructions =
    foldl (flip ($)) initialGrid

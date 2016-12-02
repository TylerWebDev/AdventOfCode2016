module Main where

import Support
import Instruction
import Step

-- Run a part of the problem
run :: ([Instruction] -> IO ()) -> IO ()
run part = do
    maybeInstructions <- stringToInstructionList <$> readFile "./input.txt"
    case maybeInstructions of
        Just instructions ->
            part instructions
        Nothing ->
            print "Parse Error"

-- Advent of Code : Day 1 Part 1
partOne :: [Instruction] -> IO ()
partOne instructions =
    print $ taxicabDistanceStep initialStep (stepDestination instructions)

-- Advent of Code : Day 1 Part 2
partTwo :: [Instruction] -> IO ()
partTwo instructions =
    let
        stepsTaken =
            initialStep : ((tupleifyList . stepThrough $ instructions) >>= uncurry extrapolate)
    in
        case firstDuplicateIn stepsTaken of
            Just duplicate ->
                print $ taxicabDistanceStep initialStep duplicate
            Nothing ->
                print "No duplicate steps encountered."

main :: IO ()
main = do
    run partOne
    run partTwo

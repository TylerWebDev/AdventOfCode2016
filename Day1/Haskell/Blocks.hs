module Blocks where

import Data.Char (isSpace)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

-- Encodes an instruction to turn left or right
data Turn
    = TurnLeft
    | TurnRight
    deriving (Show)

-- Encodes a heading that Turn can modify, cardinal directions
data Direction
    = North
    | South
    | East
    | West
    deriving (Eq, Show)

-- Encodes an instruction: A direction to turn and a magnitude to move
data Instruction =
    Instruction Turn Int
    deriving (Show)

-- Alias for a vector point (x, y)
type Position =
    (Int, Int)

-- Encodes a snapshot in time of a heading and a location
data Step = Step
    { stepDir :: Direction
    , stepLoc :: Position
    } deriving (Show)

-- Steps are equal if their location is equal; direction doesn't matter
instance Eq Step where
    stepA == stepB =
        stepLoc stepA == stepLoc stepB

-- Step A is <= Step B if Location A <= Location B; direction doesn't matter
instance Ord Step where
    stepA <= stepB =
        stepLoc stepA <= stepLoc stepB

-- Remove whitespace from beginning and end of a string
trim :: String -> String
trim =
    f . f
        where f = reverse . dropWhile isSpace

-- Parse a character into a Turn, or fail
charToTurn :: Char -> Maybe Turn
charToTurn 'L' =
    Just TurnLeft
charToTurn 'R' =
    Just TurnRight
charToTurn _ =
    Nothing

-- Parse a string into an instruction, or fail
stringToInstruction :: String -> Maybe Instruction
stringToInstruction [] =
    Nothing
stringToInstruction (turnString:countString) =
    Instruction
        <$> charToTurn turnString
        <*> Just (read countString)

-- Parse a string into a list of instructions, or fail
stringToInstructionList :: String -> Maybe [Instruction]
stringToInstructionList =
    mapM (stringToInstruction . trim) . splitOn ","

-- This is where we start
initialStep :: Step
initialStep =
    Step North (0, 0)

-- Given a Direction and a Turn, calculate a 90 degree turn in that direction
calcNewDirection :: Direction -> Turn -> Direction
calcNewDirection North TurnLeft =
    West
calcNewDirection North TurnRight =
    East
calcNewDirection East TurnLeft =
    North
calcNewDirection East TurnRight =
    South
calcNewDirection South TurnLeft =
    East
calcNewDirection South TurnRight =
    West
calcNewDirection West TurnLeft =
    South
calcNewDirection West TurnRight =
    North

-- Given a Direction, Position, and Magnitude, determine a new position
calcNewPosition :: Direction -> Position -> Int -> Position
calcNewPosition North (x, y) magnitude =
    (x, y + magnitude)
calcNewPosition East (x, y) magnitude =
    (x + magnitude, y)
calcNewPosition South (x, y) magnitude =
    (x, y - magnitude)
calcNewPosition West (x, y) magnitude =
    (x - magnitude, y)

-- Step forward in time by following a given Instruction
stepForward :: Step -> Instruction -> Step
stepForward Step {stepDir = startingDirection, stepLoc = startingPosition} (Instruction turnDirection moveMagnitude) =
    Step endingDirection endingPosition
        where
            endingDirection :: Direction
            endingDirection =
                calcNewDirection startingDirection turnDirection
            endingPosition :: Position
            endingPosition =
                calcNewPosition endingDirection startingPosition moveMagnitude

-- Step through a list of Instructions until there are none left
stepThrough :: [Instruction] -> [Step]
stepThrough =
    scanl stepForward initialStep

-- Step through a list of Instructions then return the final destination
stepDestination :: [Instruction] -> Step
stepDestination =
    last . stepThrough

-- Calculates the distance on a grid between two vector Positions
taxicabDistance :: Position -> Position -> Int
taxicabDistance (p1, p2) (q1, q2) =
    abs (p1 - q1) + abs (p2 - q2)

-- Calculates the distance on a grid between two Steps
taxicabDistanceStep :: Step -> Step -> Int
taxicabDistanceStep stepP stepQ =
    taxicabDistance (stepLoc stepP) (stepLoc stepQ)

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

-- Generate a range of integers between two values; can be increasing or decreasing
generateRange :: Int -> Int -> [Int]
generateRange a b
    | b < a =
        [a, (a - 1) .. b]
    | otherwise =
        [a, (a + 1) .. b]

-- Given a starting Step and an ending Step, calculate all the steps in between
extrapolate :: Step -> Step -> [Step]
extrapolate
    Step {stepLoc = (startX, startY)}
    Step {stepDir = direction, stepLoc = (endX, endY)}
        | direction == North || direction == South =
            tail [ Step direction (endX, y) | y <- generateRange startY endY ]
        | otherwise =
            tail [ Step direction (x, endY) | x <- generateRange startX endX ]

-- Turn a list into pairs, grouping each element with the next. [1, 2, 3] == [(1, 2), (2, 3)]
tupleifyList :: [a] -> [(a, a)]
tupleifyList [] =
    []
tupleifyList [_] =
    []
tupleifyList (x:xs) =
    (x, head xs) : tupleifyList xs

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

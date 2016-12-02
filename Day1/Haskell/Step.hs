module Step where

import Support
import Instruction

-- Encodes a heading that Turn can modify, cardinal directions
data Direction
    = North
    | South
    | East
    | West
    deriving (Eq, Show)

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

-- This is where we start
initialStep :: Step
initialStep =
    Step North (0, 0)

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

-- Given a starting Step and an ending Step, calculate all the steps in between
extrapolate :: Step -> Step -> [Step]
extrapolate
    Step {stepLoc = (startX, startY)}
    Step {stepDir = direction, stepLoc = (endX, endY)}
        | direction == North || direction == South =
            tail [ Step direction (endX, y) | y <- generateRange startY endY ]
        | otherwise =
            tail [ Step direction (x, endY) | x <- generateRange startX endX ]

-- Calculates the distance on a grid between two Steps
taxicabDistanceStep :: Step -> Step -> Int
taxicabDistanceStep stepP stepQ =
    taxicabDistance (stepLoc stepP) (stepLoc stepQ)

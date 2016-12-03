module Instruction where

import Support
import Data.List.Split (splitOn)

-- Encodes an instruction to turn left or right
data Turn
    = TurnLeft
    | TurnRight
    deriving (Show)

-- Encodes an instruction: A direction to turn and a magnitude to move
data Instruction =
    Instruction Turn Int
    deriving (Show)

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

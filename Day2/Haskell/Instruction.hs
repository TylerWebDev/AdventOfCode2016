module Instruction where

data Instruction
    = U
    | L
    | D
    | R
    deriving (Show)

charToInstruction :: Char -> Maybe Instruction
charToInstruction 'U' = Just U
charToInstruction 'L' = Just L
charToInstruction 'D' = Just D
charToInstruction 'R' = Just R
charToInstruction _ =
    Nothing

stringToInstructionList :: String -> Maybe [Instruction]
stringToInstructionList =
    mapM charToInstruction

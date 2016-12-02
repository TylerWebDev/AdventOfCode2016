module Main where

import Instruction
import qualified Keypad
import qualified KeypadHard

run :: (a -> Char) -> (a -> [[Instruction]] -> [a]) -> a -> IO ()
run toChar walkFn start = do
    maybeInstructionSet <- (mapM stringToInstructionList . lines) <$> readFile "./input.txt"
    case maybeInstructionSet of
        Just instructionSet ->
            print . map toChar $ walkFn start instructionSet
        Nothing ->
            print "Parse Error"

partOne :: IO ()
partOne =
    run Keypad.keyToChar Keypad.walkThroughFrom Keypad.K5

partTwo :: IO ()
partTwo =
    run KeypadHard.keyToChar KeypadHard.walkThroughFrom KeypadHard.HK5

main :: IO ()
main = do
    partOne
    partTwo

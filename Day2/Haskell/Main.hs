module Main where

import Instruction
import Keypad
import qualified EasyKey
import qualified HardKey

run :: (Show a, Keypad a) => a -> IO ()
run start = do
    maybeInstructionSet <- (mapM stringToInstructionList . lines) <$> readFile "./input.txt"
    case maybeInstructionSet of
        Just instructionSet ->
            putStrLn . concatMap show . walkThroughFrom start $ instructionSet
        Nothing ->
            putStrLn "Parse Error"

main :: IO ()
main = do
    run EasyKey.K5
    run HardKey.HK5

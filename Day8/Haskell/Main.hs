module Main where

import Grid
import Parse

run :: ([Instruction] -> IO ()) -> IO ()
run part = do
    parsedInstructions <- mapM parseInstruction . lines <$> readFile "./input.txt"
    case parsedInstructions of
        Left err ->
            print err
        Right instructions ->
            part instructions

partOne :: [Instruction] -> IO ()
partOne instructions =
    print . countOn $ runInstructions instructions

partTwo :: [Instruction] -> IO ()
partTwo instructions =
    putStrLn . showGrid $ runInstructions instructions

main :: IO ()
main = do
    run partOne
    run partTwo

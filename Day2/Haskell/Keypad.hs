module Keypad where

import Instruction

class Keypad a where
    step :: a -> Instruction -> a

stepToEndFrom :: Keypad a => a -> [Instruction] -> a
stepToEndFrom =
    foldl step

walkThroughFrom :: Keypad a => a -> [[Instruction]] -> [a]
walkThroughFrom start =
    tail . scanl stepToEndFrom start

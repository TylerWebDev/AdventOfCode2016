module Keypad where

import Instruction

data Key =
    K1 | K2 | K3 | K4 | K5 | K6 | K7 | K8 | K9
    deriving (Show)

keyToChar :: Key -> Char
keyToChar K1 = '1'
keyToChar K2 = '2'
keyToChar K3 = '3'
keyToChar K4 = '4'
keyToChar K5 = '5'
keyToChar K6 = '6'
keyToChar K7 = '7'
keyToChar K8 = '8'
keyToChar K9 = '9'

step :: Key -> Instruction -> Key
step K1 U = K1
step K1 L = K1
step K1 D = K4
step K1 R = K2
step K2 U = K2
step K2 L = K1
step K2 D = K5
step K2 R = K3
step K3 U = K3
step K3 L = K2
step K3 D = K6
step K3 R = K3
step K4 U = K1
step K4 L = K4
step K4 D = K7
step K4 R = K5
step K5 U = K2
step K5 L = K4
step K5 D = K8
step K5 R = K6
step K6 U = K3
step K6 L = K5
step K6 D = K9
step K6 R = K6
step K7 U = K4
step K7 L = K7
step K7 D = K7
step K7 R = K8
step K8 U = K5
step K8 L = K7
step K8 D = K8
step K8 R = K9
step K9 U = K6
step K9 L = K8
step K9 D = K9
step K9 R = K9

stepThroughFrom :: Key -> [Instruction] -> [Key]
stepThroughFrom =
    scanl step

stepToEndFrom :: Key -> [Instruction] -> Key
stepToEndFrom =
    foldl step

walkThroughFrom :: Key -> [[Instruction]] -> [Key]
walkThroughFrom start =
    tail . scanl stepToEndFrom start

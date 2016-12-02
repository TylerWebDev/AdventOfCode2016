module KeypadHard where

import Instruction

data HardKey =
    HK1 | HK2 | HK3 | HK4 | HK5 | HK6 | HK7 | HK8 | HK9 | HKA | HKB | HKC | HKD
    deriving (Show)

keyToChar :: HardKey -> Char
keyToChar HK1 = '1'
keyToChar HK2 = '2'
keyToChar HK3 = '3'
keyToChar HK4 = '4'
keyToChar HK5 = '5'
keyToChar HK6 = '6'
keyToChar HK7 = '7'
keyToChar HK8 = '8'
keyToChar HK9 = '9'
keyToChar HKA = 'A'
keyToChar HKB = 'B'
keyToChar HKC = 'C'
keyToChar HKD = 'D'

step :: HardKey -> Instruction -> HardKey
step HK1 U = HK1
step HK1 L = HK1
step HK1 D = HK3
step HK1 R = HK1
step HK2 U = HK2
step HK2 L = HK2
step HK2 D = HK6
step HK2 R = HK3
step HK3 U = HK1
step HK3 L = HK2
step HK3 D = HK7
step HK3 R = HK4
step HK4 U = HK4
step HK4 L = HK3
step HK4 D = HK8
step HK4 R = HK4
step HK5 U = HK5
step HK5 L = HK5
step HK5 D = HK5
step HK5 R = HK6
step HK6 U = HK2
step HK6 L = HK5
step HK6 D = HKA
step HK6 R = HK7
step HK7 U = HK3
step HK7 L = HK6
step HK7 D = HKB
step HK7 R = HK8
step HK8 U = HK4
step HK8 L = HK7
step HK8 D = HKC
step HK8 R = HK9
step HK9 U = HK9
step HK9 L = HK8
step HK9 D = HK9
step HK9 R = HK9
step HKA U = HK6
step HKA L = HKA
step HKA D = HKA
step HKA R = HKB
step HKB U = HK7
step HKB L = HKA
step HKB D = HKD
step HKB R = HKC
step HKC U = HK8
step HKC L = HKB
step HKC D = HKC
step HKC R = HKC
step HKD U = HKB
step HKD L = HKD
step HKD D = HKD
step HKD R = HKD

stepThroughFrom :: HardKey -> [Instruction] -> [HardKey]
stepThroughFrom =
    scanl step

stepToEndFrom :: HardKey -> [Instruction] -> HardKey
stepToEndFrom =
    foldl step

walkThroughFrom :: HardKey -> [[Instruction]] -> [HardKey]
walkThroughFrom start =
    tail . scanl stepToEndFrom start

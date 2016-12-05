module Password where

import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.Hash.MD5
import qualified Data.Sequence as Seq

startsWithZeroes :: String -> Bool
startsWithZeroes str =
    take 5 str == "00000"

hashSequence :: String -> [String]
hashSequence key =
    map md5s [ Str $ key ++ show x | x <- [1..] :: [Int] ]

calculatePassword :: String -> String
calculatePassword =
    map (!! 5) . take 8 . filter startsWithZeroes . hashSequence

extractPosValTuple :: String -> (Int, Char)
extractPosValTuple hash =
    (digitToInt (hash !! 5), hash !! 6)

updatePassword :: Seq.Seq Char -> (Int, Char) -> Seq.Seq Char
updatePassword pass (pos, val)
    | pos + 1 > Seq.length pass =
        pass
    | Seq.index pass pos /= '_' =
        pass
    | otherwise =
        Seq.update pos val pass

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) =
    x : if p x then takeWhileInclusive p xs else []

calculateHardPassword :: String -> String
calculateHardPassword =
    toList
        . last
        . takeWhileInclusive (elem '_')
        . scanl updatePassword (Seq.fromList "________")
        . map extractPosValTuple
        . filter startsWithZeroes
        . hashSequence

partOne :: IO ()
partOne =
    putStrLn $ calculatePassword "uqwqemis"

partTwo :: IO ()
partTwo =
    putStrLn $ calculateHardPassword "uqwqemis"

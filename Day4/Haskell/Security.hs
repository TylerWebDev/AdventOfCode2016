{-# LANGUAGE TemplateHaskell #-}

module Security where

import Room
import Parse
import Control.Arrow
import Data.List
import Data.Char
import Control.Lens hiding (element)

-- Generate the following lenses:
-- [name, sector, checksum]
makeLenses ''Room

frequency :: Ord a => [a] -> [(Int, a)]
frequency =
    map (length &&& head) . group . sort

order :: Ord a => a -> a -> Ordering
order b a
    | a < b = LT
    | a > b = GT
    | otherwise = EQ

sortNumericallyThenAlphabetically :: (Int, Char) -> (Int, Char) -> Ordering
sortNumericallyThenAlphabetically (numA, letterA) (numB, letterB)
    -- Occurrences not equal
    | numA /= numB =
        order numA numB
    -- Occurences equal, letters not equal
    | letterA /= letterB =
        order letterB letterA
    -- Everything equal
    | otherwise =
        EQ

calculateSecurityChecksum :: String -> String
calculateSecurityChecksum =
    take 5 . map snd . sortBy sortNumericallyThenAlphabetically . frequency

calculateRoomChecksum :: Room -> String
calculateRoomChecksum =
    calculateSecurityChecksum . filter (/= '-') . _name

validateRoomChecksum :: Room -> Bool
validateRoomChecksum room =
    calculateRoomChecksum room == _checksum room

nextChar :: Char -> Char
nextChar 'z' = 'a'
nextChar c =
    chr (ord c + 1)

decypherChar :: Int -> Char -> Char
decypherChar _ '-' =
    ' '
decypherChar nTimes fromStart =
    last . take (nTimes + 1) $ iterate nextChar fromStart

decodeRoom :: Room -> Room
decodeRoom room =
    over name (map $ decypherChar (_sector room)) room

run :: ([Room] -> IO ()) -> IO ()
run part = do
    parsedRoomList <- mapM parseRoom . lines <$> readFile "./input.txt"
    case parsedRoomList of
        Right roomList ->
            part roomList
        Left err ->
            print err

partOne :: [Room] -> IO ()
partOne =
    print . foldl (\total room -> total + _sector room) 0 . filter validateRoomChecksum

partTwo :: [Room] -> IO ()
partTwo =
    mapM_ (print . decodeRoom)

main :: IO ()
main =
    run partTwo

module Triangle where

import Data.List.Split

type Triple =
    (Int, Int, Int)

isTriangle :: Triple -> Bool
isTriangle (a, b, c) =
    a + b > c && b + c > a && c + a > b

tuple3 :: [a] -> Maybe (a, a, a)
tuple3 [a, b, c] =
    Just (a, b, c)
tuple3 _ =
    Nothing

strToTriple :: String -> Maybe Triple
strToTriple =
    tuple3 . map read . words

chunkToTriangle :: (Triple, Triple, Triple) -> [Triple]
chunkToTriangle ((a1, b1, c1), (a2, b2, c2), (a3, b3, c3)) =
    [ (a1, a2, a3)
    , (b1, b2, b3)
    , (c1, c2, c3)
    ]

partOne :: IO ()
partOne = do
    maybeTriangles <- mapM strToTriple . lines <$> readFile "./input.txt"
    case maybeTriangles of
        Just triangles ->
            print . length . filter isTriangle $ triangles
        Nothing ->
            putStrLn "Parse Error"

partTwo :: IO ()
partTwo = do
    maybeTriangles <- mapM strToTriple . lines <$> readFile "./input.txt"
    case maybeTriangles of
        Just triangles ->
            case mapM tuple3 . chunksOf 3 $ triangles of
                Just transverse ->
                    print . length . filter isTriangle $ transverse >>= chunkToTriangle
                Nothing ->
                    putStrLn "Parse Error"
        Nothing ->
            putStrLn "Parse Error"

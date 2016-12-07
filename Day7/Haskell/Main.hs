module Main where

import IPV7
import TLS
import SSL

partOne :: IO ()
partOne = do
    addresses <- mapM parseIPV7ForTLS . lines <$> readFile "./input.txt"
    print $ length . filter ipSupportsTLS <$> addresses

partTwo :: IO ()
partTwo = do
    addresses <- mapM parseIPV7ForSSL . lines <$> readFile "./input.txt"
    case addresses of
        Left _ ->
            print "Parse Error"
        Right a ->
            print $ length . filter ipSupportsSSL $ a

main :: IO ()
main = do
    partOne
    partTwo

module SSL where

import IPV7
import Control.Arrow

combinations :: [a] -> [b] -> [(a, b)]
combinations as bs =
    [ (a, b) | a <- as, b <- bs ]

palindromePairIsSSLCompatible :: (IPV7Component, IPV7Component) -> Bool
palindromePairIsSSLCompatible (Palindrome [a, b, _], Palindrome [b', a', _])
    | a == a' && b == b' = True
    | otherwise = False
palindromePairIsSSLCompatible _ =
    False

ipSupportsSSL :: IPV7 -> Bool
ipSupportsSSL =
    any palindromePairIsSSLCompatible . uncurry combinations . (concatMap getPalindromes . supernetComponents &&& concatMap getPalindromes . hypernetComponents)
    where
        getPalindromes = filter isPalindrome . findTriplePalindromes

findTriplePalindromes :: IPV7 -> [IPV7Component]
findTriplePalindromes (a@(Letter a') : b@(Letter b') : c@(Letter c') : xs)
    | a' == c' =
        Palindrome [a', b', c'] : findTriplePalindromes rest
    | otherwise =
        a : findTriplePalindromes rest
    where
        rest = b:c:xs
findTriplePalindromes _ =
    []

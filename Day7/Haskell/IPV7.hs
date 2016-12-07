module IPV7 where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>), many)

type IPV7 =
    [IPV7Component]

data IPV7Component
    = Letter Char
    | Palindrome String
    | Nested [IPV7Component]
    deriving (Show)

abbaPalindrome :: Parser IPV7Component
abbaPalindrome = do
    letterA <- letter
    letterB <- noneOf [letterA]
    letterB' <- char letterB
    letterA' <- char letterA
    return $ Palindrome [letterA, letterB, letterB', letterA']

tlsAtom :: Parser IPV7Component
tlsAtom =
    try abbaPalindrome <|> (Letter <$> letter)

sslAtom :: Parser IPV7Component
sslAtom =
    Letter <$> letter

tlsNestedAtom :: Parser IPV7Component
tlsNestedAtom =
    char '[' *> (Nested <$> many tlsAtom) <* char ']'

sslNestedAtom :: Parser IPV7Component
sslNestedAtom =
    char '[' *> (Nested <$> many sslAtom) <* char ']'

ipv7Tls :: Parser IPV7
ipv7Tls =
    many (tlsNestedAtom <|> tlsAtom)

ipv7Ssl :: Parser IPV7
ipv7Ssl =
    many (sslNestedAtom <|> sslAtom)

parseIPV7ForTLS :: String -> Either ParseError IPV7
parseIPV7ForTLS =
    parse ipv7Tls ""

parseIPV7ForSSL :: String -> Either ParseError IPV7
parseIPV7ForSSL =
    parse ipv7Ssl ""

isSupernet :: IPV7Component -> Bool
isSupernet (Nested _) =
    False
isSupernet _ =
    True

isHypernet :: IPV7Component -> Bool
isHypernet =
    not . isSupernet

isPalindrome :: IPV7Component -> Bool
isPalindrome (Palindrome _) =
    True
isPalindrome _ =
    False

supernetComponents :: IPV7 -> [[IPV7Component]]
supernetComponents xs
    | null rest =
        [takeWhile isSupernet xs]
    | otherwise =
        takeWhile isSupernet xs : supernetComponents (tail rest)
    where
        rest = dropWhile isSupernet xs

hypernetComponents :: IPV7 -> [[IPV7Component]]
hypernetComponents net =
    map extractNested $ filter isHypernet net
    where
        extractNested (Nested n) =
            n
        extractNested _ =
            []

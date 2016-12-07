module Abba where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>), many)

newtype IPV7 =
    IPV7 [IPV7Component] deriving (Show)

data IPV7Component
    = Letter Char
    | Palindrome String
    | Nested [IPV7Component]
    deriving (Show)

data TLSSupport
    = Inconsequential
    | Yes
    | No
    deriving (Show)

instance Monoid TLSSupport where
    mempty =
        Inconsequential
    mappend Inconsequential n =
        n
    mappend No _ =
        No
    mappend Yes No =
        No
    mappend Yes _ =
        Yes

palindrome :: Parser IPV7Component
palindrome = do
    letterA <- letter
    letterB <- noneOf [letterA]
    letterB' <- char letterB
    letterA' <- char letterA
    return $ Palindrome [letterA, letterB, letterB', letterA']

atom :: Parser IPV7Component
atom =
    try palindrome <|> (Letter <$> letter)

nestedAtom :: Parser IPV7Component
nestedAtom =
    char '[' *> (Nested <$> many atom) <* char ']'

ipv7 :: Parser IPV7
ipv7 =
    IPV7 <$> many (nestedAtom <|> atom)

componentsSupportTLS :: IPV7Component -> TLSSupport
componentsSupportTLS (Letter _) =
    Inconsequential
componentsSupportTLS (Palindrome _) =
    Yes
componentsSupportTLS (Nested n) =
    mconcat (map nestedComponentsSupportTLS n)

nestedComponentsSupportTLS :: IPV7Component -> TLSSupport
nestedComponentsSupportTLS (Letter _) =
    Inconsequential
nestedComponentsSupportTLS _ =
    No

ipSupportsTLS :: IPV7 -> Bool
ipSupportsTLS (IPV7 ip) =
    case mconcat (map componentsSupportTLS ip) of
        Yes ->
            True
        _ ->
            False

partOne :: IO ()
partOne = do
    addresses <- mapM (parse ipv7 "") . lines <$> readFile "./input.txt"
    print $ length . filter ipSupportsTLS <$> addresses

module Parse (parseRoom) where

import Room
import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>), many)

encryptedNameLetter :: Parser Char
encryptedNameLetter =
    letter <|> char '-'

encryptedName :: Parser String
encryptedName =
    many encryptedNameLetter

securitySectorNumber :: Parser Int
securitySectorNumber =
    read <$> count 3 digit

securityChecksum :: Parser String
securityChecksum =
    char '[' *> count 5 letter <* char ']'

securityRoom :: Parser Room
securityRoom =
    Room
        <$> (init <$> encryptedName) -- I'm sure there's a better way to do this...
        <*> securitySectorNumber
        <*> securityChecksum

parseRoom :: String -> Either ParseError Room
parseRoom =
    parse securityRoom ""

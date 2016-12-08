module Parse where

import Grid
import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>), many)

parseRect :: Parser Instruction
parseRect = try $ do
    string "rect "
    parsedX <- read <$> manyTill digit (char 'x')
    parsedY <- read <$> many digit
    return (rect parsedX parsedY)

parseRow :: Parser Instruction
parseRow = try $ do
    string "rotate row y="
    parsedRow <- read <$> many digit
    string " by "
    parsedAmt <- read <$> many digit
    return (rotateRow parsedRow parsedAmt)

parseCol :: Parser Instruction
parseCol = try $ do
    string "rotate column x="
    parsedCol <- read <$> many digit
    string " by "
    parsedAmt <- read <$> many digit
    return (rotateCol parsedCol parsedAmt)

parseInstruction :: String -> Either ParseError Instruction
parseInstruction =
    parse (parseRect <|> parseRow <|> parseCol) ""

module Parse where

import Control.Monad
import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>), many)

type CompressionString =
    [CompressionAtom]

data CompressionAtom
    = Single Char
    | Compressed Int [CompressionAtom]
    deriving (Show)

whitespace :: Parser String
whitespace =
    many (oneOf " \t\n")

atomNumber :: Parser Int
atomNumber =
    read <$> many1 digit

atomSingle :: Parser CompressionAtom
atomSingle =
    Single <$> (anyChar <* whitespace)

atomCompressed :: Parser a -> (Int -> [a] -> b) -> Parser b
atomCompressed combi fn = try $ do
    char '('
    takeNAtoms <- atomNumber
    char 'x'
    nTimes <- atomNumber
    char ')'
    nextNAtoms <- replicateM takeNAtoms combi
    return $ fn nTimes nextNAtoms

atomCompressedSimple :: Parser CompressionAtom
atomCompressedSimple =
    atomCompressed atomSingle Compressed

atomCompressedComplex :: Parser CompressionAtom
atomCompressedComplex =
    atomCompressed (anyChar <* whitespace) (\nTimes nextNAtoms -> Compressed nTimes $ either (const []) id $ parseCompressionComplex nextNAtoms)

atomSimple :: Parser CompressionAtom
atomSimple =
    atomCompressedSimple <|> atomSingle

atomComplex :: Parser CompressionAtom
atomComplex =
    atomCompressedComplex <|> atomSingle

parseCompression :: String -> Either ParseError CompressionString
parseCompression =
    parse (many atomSimple) "Compression String"

parseCompressionComplex :: String -> Either ParseError CompressionString
parseCompressionComplex =
    parse (many atomComplex) "Compression String - Complex"

simpleCompressionLength :: CompressionAtom -> Int
simpleCompressionLength (Single _) = 1
simpleCompressionLength (Compressed n contents) =
    n * length contents

complexCompressionLength :: CompressionAtom -> Int
complexCompressionLength (Single _) = 1
complexCompressionLength (Compressed n contents) =
    n * (sum . map complexCompressionLength $ contents)

uncompressedStringLengthUsing  :: (CompressionAtom -> Int) -> CompressionString -> Int
uncompressedStringLengthUsing  fn =
    sum . map fn

partOne :: IO ()
partOne = do
    inputString <- readFile "./input.txt"
    print $ uncompressedStringLengthUsing simpleCompressionLength <$> parseCompression inputString

partTwo :: IO ()
partTwo = do
    inputString <- readFile "./input.txt"
    print $ uncompressedStringLengthUsing complexCompressionLength <$> parseCompressionComplex inputString

module Lib.Parser (Parser, parse, parse', parseAll, parseAll', intP) where

import qualified Text.Parsec as P

type Parser = P.Parsec String ()

parse :: Parser a -> String -> Either P.ParseError a
parse parser = P.parse parser ""

parse' :: Parser a -> String -> a
parse' parser = fromRight . parse parser

parseAll :: Parser a -> [String] -> Either P.ParseError [a]
parseAll parser = sequence <$> map (parse parser)

parseAll' :: Parser a -> [String] -> [a]
parseAll' parser = fromRight . parseAll parser

fromRight :: (Show l) => Either l r -> r
fromRight (Left l) = error $ show l
fromRight (Right r) = r

intP :: Parser Int
intP = read <$> P.many1 P.digit <* P.spaces

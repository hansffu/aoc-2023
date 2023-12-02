module Lib.Parser (Parser, parse, parseAll, intP) where

import qualified Text.Parsec as P

type Parser = P.Parsec String ()

parse :: Parser a -> String -> Either P.ParseError a
parse parser = P.parse parser ""

parseAll :: Parser a -> [String] -> Either P.ParseError [a]
parseAll parser = sequence <$> map (parse parser)

intP :: Parser Int
intP = read <$> P.many1 P.digit

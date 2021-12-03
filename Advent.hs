module Advent (Parser, sc, lexeme, number, runParser',
                getErrs, getParsed, pairs, showParsed,
                showErrs) where

import Data.List
import Data.Functor
import Control.Applicative
import qualified Data.Either as Either
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad
import Text.Megaparsec.Error

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty 

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

number = L.signed sc L.decimal

arbString :: Parser String 
arbString = lexeme $ manyTill anySingle spaceChar 

execParser p = runParser p ""

getErrs p ls   = errorBundlePretty .
                  Either.fromLeft undefined  . execParser p <$> ls

getParsed :: Show a => Parser a -> [String] -> [a]
getParsed p ls =  Either.fromRight undefined . execParser p <$> ls

showParsed :: Show a => Parser a -> [String] -> IO [()]
showParsed p ls = sequence $ print <$> getParsed p ls
showErrs   :: Parser a -> [String] -> IO [()]
showErrs p ls  = sequence $ print <$> getErrs p ls

pairs l = zip l $ tail l


module Advent (Parser, sc, lexeme, number, eolf, arbString,
                runParser', getErrs, getParsed, showParsed,
                showErrs, pairs, composeReverse,
                groupWithLength, compose, times) where

import Data.List
import Data.Functor
import Control.Applicative
import qualified Data.Either as Either
import Text.Megaparsec hiding (some, many)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad
import Text.Megaparsec.Error

type Parser = Parsec Void String

sc :: Parser ()
sc  = L.space (void $ some (char ' ' <|> char '\t')) empty empty 
scn :: Parser ()
scn = L.space space1 empty empty 

eolf :: Parser ()
eolf = void $ (many eol) <* eof

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
showErrs p ls  = sequence $ putStrLn <$> getErrs p ls

pairs l = zip l $ tail l

composeReverse :: Foldable t => t (b -> b) -> b -> b
composeReverse = foldr (flip (.)) id
 
compose :: Foldable t => t (b -> b) -> b -> b
compose = foldr (.) id

--means "cardinality" when xs sorted
groupWithLength xs = (\x -> (head x, length x)) <$> group xs

-- via glguy:
-- https://github.com/glguy/advent2020/blob/ed247131684efa699bd34b846b5d939e9db749fc/common/Advent.hs#L134
times :: Int -> (a -> a) -> a -> a
times n f x
  | n <= 0    = x
  | otherwise = times (n-1) f $! f x


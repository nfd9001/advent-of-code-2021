{-# LANGUAGE TupleSections #-}
import Data.List
import Data.Functor
import Control.Applicative
import qualified Data.Either as Either
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char
import Control.Monad.Combinators hiding ((<|>), optional, empty, many, some)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad
import Text.Megaparsec.Error
import Data.Maybe
import Advent

-------------------------------------------------------------------------------
--Data
-------------------------------------------------------------------------------
type Called = [Integer]
type Card   = [[(Integer, Bool)]]

parseCalled :: Parser Called
parseCalled = number `sepBy` char ',' <* eol

parseCard :: Parser Card
parseCard = sepEndBy (hspace *> some ((,False) <$> lexeme number)) eol

parseInput :: Parser (Called, [Card])
parseInput = do
    called <- parseCalled 
    eol
    cards  <- parseCard `sepEndBy` eol
    eof
    pure (called, cards)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

callNumber :: Integer -> Card -> Card
callNumber num card = fmap (\orig@(square, _)->
                        if square == num
                        then (square, True)
                        else orig)
                      <$> card

isCardWinner :: Card -> Bool
isCardWinner c = or $ all snd <$> (c ++ transpose c)

calcScore :: Integer -> Card -> Integer
calcScore called card = (*called) $ sum $
     fst <$> filter (not . snd) (concat card)

getAns :: Called -> [Card] -> Integer
getAns (n:ns) cards = let
    cards'  = callNumber n <$> cards
    winners = filter isCardWinner cards'
    in if not $ null winners 
       then calcScore n (head winners) 
       else getAns ns cards'

getAns2 :: Called -> [Card] -> Integer
getAns2 (n:ns) cards = let
    cards'  = callNumber n <$> cards
    (won, notwon) = partition isCardWinner cards'
    in if null notwon
       then calcScore n (head won) 
       else getAns2 ns notwon

-------------------------------------------------------------------------------
--Go
-------------------------------------------------------------------------------
main = do
    f <- readFile "inputs/day4.txt"
    --TODO: change Parser to parse Chars instead of Strings
    let [(called, cards)] = getParsed parseInput [f]
    print $ getAns  called cards
    print $ getAns2 called cards


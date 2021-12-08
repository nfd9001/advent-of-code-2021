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
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

-- Data
parseCrabs :: Parser [Int]
parseCrabs = (fromIntegral <$> number) `sepBy` char ',' <* eolf

-- Logic
cost  x y = abs $ x - y

cost2 x y = (n*(n + 1)) `div` 2 where n = cost x y

-- all inputs are even but y'know.
median xs = let
    l = length xs
    ld = l `div` 2
    odd = l `mod` 2 == 1
    mid = xs!!ld
    othermid = xs!!(ld-1)
    in if odd then mid else mid + ((othermid - mid) `div` 2)

-- Go
main = do
    f <- readFile "inputs/day7.txt"
    let [crabs] = sort <$> getParsed parseCrabs [f]
    let m = median crabs
    print $ sum $ (cost m) <$> crabs
    
    -- not outrageously happy with a bruteforce pt. 2...
    let range = [minimum crabs..maximum crabs]
    let costs = do
        pos <- range
        pure $ sum $ cost2 pos <$> crabs
    print $ minimum costs


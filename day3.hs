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
import Advent

import Data.Bits

parseRow :: Parser [Int]
parseRow = Text.Megaparsec.many ((char '1' $> 1) <|> (char '0' $> 0)) <* eof

intify :: [Int] -> Int
intify ns = let
    nspos = zip (reverse ns) [0..]
    idxes = snd <$> filter (\x -> fst x == 1) nspos
    in foldl' setBit 0 idxes

findBit op rows col = let
    tp = transpose rows
    (zeroes, ones) = span (==0) $ sort $ tp!!col
    in if length zeroes `op` length ones then 1 else 0

reduceRows criteria col rows = let
    bit = criteria rows col
    reduced = filter (\row -> (row!!col) == bit) rows 
    in if length rows == 1 then rows else reduced
    
main = do
    f <- readFile "inputs/day3.txt"
    let ls = lines f
    let rows = getParsed parseRow ls  
    let wid = length $ head rows

    -- pt 1
    let tp = transpose rows
    let len = length $ head tp

    let stp = sort <$> tp
    let nzeroes = length . takeWhile (==0) <$> stp

    -- happens to work because there are 1000 entries
    let mcbits = (\x -> if x > len `div` 2 then 0 else 1) <$> nzeroes
    let lcbits = (\x -> if x == 1 then 0 else 1) <$> mcbits
    print $ (intify mcbits) * (intify lcbits)
    -- pt. 2
    let oxfilter =  (reduceRows $ findBit (<=)) <$> [0..wid - 1]
    let co2filter = (reduceRows $ findBit (>))  <$> [0..wid - 1]
    let oxrating = head $ oxfilter `threadThroughEndos` rows
    let co2rating = head $ co2filter `threadThroughEndos` rows
    print $ (intify oxrating) * (intify co2rating)


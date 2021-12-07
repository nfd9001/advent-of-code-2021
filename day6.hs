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
parseFish :: Parser [Int]
parseFish =  (fromIntegral <$> number) `sepBy` char ',' <* eolf

-- Go
initFish :: [Int] -> IntMap Int
initFish fish =  IM.fromDistinctAscList $ groupWithLength $ sort fish

doEpoch :: IntMap Int -> IntMap Int
doEpoch fish = let
    l = IM.toList fish
    dec (0,_) = Nothing
    dec (x,n) = Just (x - 1, n)
    almostfish = IM.fromDistinctAscList $ catMaybes $ dec <$> l
    zero = find (\x -> fst x == 0) l
    addNewGen (Just (0,n)) map = IM.insertWith (+) 6 n $ IM.insertWith (+) 8 n almostfish
    addNewGen _ map = map
    in addNewGen zero almostfish

-- Logic
main = do
    f <- readFile "inputs/day6.txt"
    let [fish] = getParsed parseFish [f]
    let fishmap = initFish fish
    print $ sum $ IM.elems $ times 80 doEpoch fishmap
    print $ sum $ IM.elems $ times 256 doEpoch fishmap


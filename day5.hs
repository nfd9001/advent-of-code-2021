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

-- ===========================================================================
-- Data
-- ===========================================================================
type Point = (Integer, Integer)
type LineSegment = (Point, Point)

parsePoint :: Parser Point
parsePoint = lexeme $ do
    first <- number
    char ','
    second <- number
    pure (first, second)

parseLineSegment = do
    first <- parsePoint
    lexeme $ string "->"
    second <- parsePoint
    pure (first, second)

parseInput = parseLineSegment `sepEndBy` eol <* eolf

-- ===========================================================================
-- Logic 
-- ===========================================================================
--assuming no zero-length segs
isVert  (Normed ((x1,_),(x2,_))) = x1 == x2 
isHoriz (Normed ((_,y1),(_,y2))) = y1 == y2 
isDiag p = not $ isVert p || isHoriz p

-- these are segments, not rays.
-- vert goes low-high, horiz goes left-right,
-- diagonals go up-right or down-right
newtype Normed = Normed LineSegment
normSeg (p1, p2) = Normed (p1', p2') where [p1', p2'] = sort [p1, p2]

getHorizPoints (Normed ((x1,y1), (x2,y2))) = (,y1) <$> [x1..x2]

getVertPoints  (Normed ((x1,y1), (x2,y2))) = (x1,) <$> [y1..y2]

getDiagPoints (Normed ((x1, y1),(x2,y2)))  = if y1 < y2
    then zip [x1..x2][y1..y2]
    else zip [x1..x2][y1, y1 - 1..y2]

-- (point, overlap degree)
getPointOverlaps ps = filter (\x -> snd x > 1) $ groupWithLength $ sort ps

part1 segs = let
    segs' = normSeg <$> segs
    horiz = concat $ getHorizPoints <$> filter isHoriz segs'
    vert  = concat $ getVertPoints  <$> filter isVert  segs'
    points = horiz ++ vert
    g = getPointOverlaps points
    in length g

part2 segs = let
    segs' = normSeg <$> segs
    horiz = concat $ getHorizPoints <$> filter isHoriz segs'
    vert  = concat $ getVertPoints  <$> filter isVert  segs'
    diag  = concat $ getDiagPoints  <$> filter isDiag  segs'
    points = horiz ++ vert ++ diag
    g = getPointOverlaps points
    in length g

-- ===========================================================================
-- Go
-- ===========================================================================
main = do
    f <- readFile "inputs/day5.txt"
    let [segs] = getParsed parseInput [f]
    print $ part1 segs
    print $ part2 segs


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

-- ===========================================================================
-- Data
-- ===========================================================================

type Parser = Parsec Void String

data Command = Up Integer | Down Integer | Forward Integer deriving (Eq, Show)

sc :: Parser ()
sc = L.space space1 empty empty 
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
number = L.signed sc L.decimal

parseHeading   = lexeme $ (string "forward" $> Forward) <|> 
                 (string "up" $> Up) <|> 
                 (string "down" $> Down) 

parseCommand = parseHeading <*> number <* eof
                 
toCommand = runParser parseCommand ""

getErrs ls = Either.fromLeft undefined . toCommand <$> ls
getCommands ls = Either.fromRight undefined . toCommand <$> ls

-- ===========================================================================
-- Logic
-- ===========================================================================
runCommand (h, d) (Up x) = (h, d - x)
runCommand (h, d) (Down x) = (h, d + x)
runCommand (h, d) (Forward x) = (h + x, d)

runCommand2 (h,d,a) (Up x) = (h, d, a - x)
runCommand2 (h,d,a) (Down x) = (h, d, a + x)
runCommand2 (h,d,a) (Forward x) = (h + x, d + (x * a), a)

-- ===========================================================================
-- Go
-- ===========================================================================
main = do
    f <- readFile "inputs/day2.txt"
    let ls = lines f
    let commands = getCommands ls
    --sequence $ print . errorBundlePretty <$> getErrs ls 
    --sequence $ print <$> commands
    print $ uncurry (*) $ foldl' runCommand (0,0) commands
    print $ (\(a,b,_) -> a * b) $ foldl' runCommand2 (0,0,0) commands



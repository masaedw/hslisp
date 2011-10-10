module Parse where

import Control.Applicative ((*>), (<*))
import Data.Attoparsec
import Data.Attoparsec.Combinator
import Data.ByteString (ByteString)
import Data.Word
import Monad
import qualified Data.Attoparsec.Char8 as C8

import LispData

exprs :: Parser [SExp]
exprs = sexp `sepBy` many spaces

sexp :: Parser SExp
sexp = choice [list, number, symbol]

list :: Parser SExp
list = liftM List $ C8.char '(' *> exprs <* C8.char ')'

number :: Parser SExp
number = liftM (Number . read) $ many1 $ C8.satisfy C8.isDigit

symbol :: Parser SExp
symbol = liftM Symbol $ C8.takeWhile1 $ not . \x -> C8.isSpace x || C8.inClass "()" x

spaces :: Parser Word8
spaces = satisfy C8.isSpace_w8

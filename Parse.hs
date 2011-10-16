{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Control.Applicative ((*>), (<*))
import Data.Attoparsec
import Data.Attoparsec.Combinator
import Data.ByteString (ByteString)
import Data.Word
import Data.List (mapAccumL)
import Monad
import qualified Data.Attoparsec.Char8 as C8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8

import LispData

exprs :: Parser [SExp]
exprs = many sexp

sexp :: Parser SExp
sexp = many spaces *> choice [list, number, symbol]

list :: Parser SExp
list = liftM List $ C8.char '(' *> exprs <* C8.char ')'

number :: Parser SExp
number = liftM (Number . read) $ many1 $ C8.satisfy C8.isDigit

symbol :: Parser SExp
symbol = liftM Symbol $ C8.takeWhile1 $ not . \x -> C8.isSpace x || C8.inClass "()" x

spaces :: Parser Word8
spaces = satisfy C8.isSpace_w8

parseLisp :: String -> [SExp]
parseLisp src =
    concat $ snd $ mapAccumL parse' (C8.Done "" undefined) $ map BC8.pack $ lines src
    where
      parse' :: C8.Result SExp -> ByteString -> (C8.Result SExp, [SExp])
      parse' (Partial f) line = parse'' $ f line
      parse' (Done s _) line = parse'' $ parse sexp $ B.append s line

      parse'' :: C8.Result SExp -> (C8.Result SExp, [SExp])
      parse'' x =
          case x of
            (Fail _ a b) -> error $ show a ++ show b
            p@(Partial _) -> (p, [])
            d@(Done s r) ->
                let (n, rs) = parse' d "" in
                (n, r : rs)

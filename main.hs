{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.Enumerator
import Data.Enumerator (($$), (<==<), ($=), (=$))
import Data.Enumerator.Binary as EB
import System
import IO
import qualified Data.ByteString as BS
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Map as Map

import Parse
import LispData

x :: Monad m => E.Iteratee BS.ByteString m [SExp]
x = iterParser exprs

readSExp :: SExp -> LObject
readSExp (Symbol symbol) = LSymbol symbol
readSExp (Number num) = LNumber num
readSExp (List []) = LNil
readSExp (List ss) = LList $ Prelude.map readSExp ss

eval :: Context -> LObject -> (Context, LObject)
eval ctx e@(LNumber _) = (ctx, e)
eval ctx (LSymbol name) = (ctx, ctx Map.! name)
eval ctx (LList ((LSymbol name):[])) = (ctx, ctx Map.! name)
eval ctx e@(LList []) = (ctx, e)



main :: IO ()
main = do
  args <- getArgs
  let file = if length args > 0
             then EB.enumFile $ Prelude.head args
             else EB.enumHandle 100 stdin
  -- E.run_ $ file $$ EL.mapM_ $ BS.putStrLn
  hoge <- E.run_ $ file $$ x
  Prelude.mapM_ print hoge

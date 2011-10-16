{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.Enumerator
import Data.Enumerator (($$), (<==<), ($=), (=$))
import Data.Maybe (isJust, fromJust)
import Data.List (mapAccumL)
import IO
import System
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
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
eval ctx (LList ((LSymbol name):xs)) =
    (ctx', lapply f vs)
    where
      (ctx', vs) = mapAccumL eval ctx xs
      f = ctx' Map.! name
eval ctx e@(LList []) = (ctx, e)

lapply :: LObject -> [LObject] -> LObject
lapply (LFunc name f) os =
    case f os of
      Right x -> x
      Left x -> error x

lfunc :: String -> Subr -> LObject
lfunc name f = LFunc name f

lplus :: Subr
lplus [] = Right $ LNumber 0
lplus os =
    if all isJust nums then
        Right $ LNumber $ foldr (+) 0 $ map fromJust nums
    else
        Left "applied not number"
    where
      nums :: [Maybe Int]
      nums = map getNumber os

lminus :: Subr
lminus [(LNumber i)] = Right $ LNumber $ -i
lminus os =
    if all isJust nums then
        Right $ LNumber $ foldl1 (-) $ map fromJust nums
    else
        Left "applied not number"
    where
      nums :: [Maybe Int]
      nums = map getNumber os

initCtx :: Context
initCtx = Map.fromList $
          map f
          [ ("+", lplus)
          , ("-", lminus)
          ]
    where
      f :: (String, Subr) -> (BS.ByteString, LObject)
      f (n, s) = (BSC.pack n, lfunc n s)

main :: IO ()
main = do
  args <- getArgs
  let file = if length args > 0
             then EB.enumFile $ Prelude.head args
             else EB.enumHandle 100 stdin
  -- E.run_ $ file $$ EL.mapM_ $ BS.putStrLn
  hoge <- E.run_ $ file $$ x
  forM_ (map readSExp hoge) $ \sexp -> do
         let (ctx, result) = eval initCtx sexp
         print result

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
import qualified Data.Map as Map

import Parse
import LispData

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

--
-- Lisp 処理系は概念としては
--              read            eval             print
--    Stirng     →  [LObject]   →   [LObject]   →    IO ()
-- ソースコード      シンボル列       実行結果          印字
--
-- という構造になると思う
--

readSExp :: SExp -> LObject
readSExp (Symbol symbol) = LSymbol symbol
readSExp (Number num) = LNumber num
readSExp (List []) = LNil
readSExp (List ss) = LList $ Prelude.map readSExp ss

readLisp :: String -> [LObject]
readLisp = map readSExp . parseLisp

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  src <- readFile file
  forM_ (values src) $ \result -> do
         print result
  where
    values = snd . mapAccumL eval initCtx . readLisp

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.Enumerator
import Data.Enumerator (($$), (<==<), ($=), (=$))
import Data.Enumerator.Binary as EB
import System
import qualified Data.ByteString as BS
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import Parse

x :: Monad m => E.Iteratee BS.ByteString m [SExp]
x = iterParser exprs

main :: IO ()
main = do
  args <- getArgs
  let file = EB.enumFile $ Prelude.head args
  -- E.run_ $ file $$ EL.mapM_ $ BS.putStrLn
  hoge <- E.run_ $ file $$ x
  Prelude.mapM_ print hoge

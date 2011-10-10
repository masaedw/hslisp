module LispData where

import qualified Data.ByteString as BS
import qualified Data.Map as Map

data SExp = Symbol BS.ByteString
          | Number Int
          | List [SExp]
            deriving Show

type Context = Map.Map BS.ByteString LObject

data LObject = LSymbol BS.ByteString
             | LNumber Int
             | LList [LObject]
             | LFunc (Context -> [LObject] -> LObject)
             | LSpecial (Context -> [LObject] -> (Context, LObject))
             | LNil

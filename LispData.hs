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
             | LFunc ([LObject] -> LObject)
             | LSpecial String (Context -> [LObject] -> (Context, LObject))
             | LNil

instance Show LObject where
    show (LSymbol s) = "LSymbol " ++ show s
    show (LList os) = "LList " ++ show os
    show (LNumber n) = "LNumber " ++ show n
    show (LFunc _) = "LFunc ..."
    show (LSpecial name _) = "LSpecial " ++ name
    show (LNil) = "LNil"

module LispData where

import qualified Data.ByteString as BS
import qualified Data.Map as Map

data SExp = Symbol BS.ByteString
          | Number Int
          | List [SExp]
            deriving Show

type Context = Map.Map BS.ByteString LObject

type Result = Either String LObject
type Subr = [LObject] -> Result
type Special = (Context -> [LObject] -> (Context, Result))

data LObject = LSymbol BS.ByteString
             | LNumber Int
             | LList [LObject]
             | LFunc String Subr
             | LSpecial String Special
             | LNil

getNumber :: LObject -> Maybe Int
getNumber (LNumber i) = Just i
getNumber _ = Nothing

instance Show LObject where
    show (LSymbol s) = "LSymbol " ++ show s
    show (LList os) = "LList " ++ show os
    show (LNumber n) = "LNumber " ++ show n
    show (LFunc name _) = "LFunc " ++ name
    show (LSpecial name _) = "LSpecial " ++ name
    show (LNil) = "LNil"

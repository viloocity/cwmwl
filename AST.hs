{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}


module AST where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Data.Typeable

-- import Data.Serialize

data AST
    -- = Number Double
    = Number Integer
    | Identifier String
    | String String
    -- | Operation BinOp AST AST
    | Query String
    deriving (Show, Eq, Generic)

data Tuple = Tuple {cmd :: String,
                    cid :: AST, 
                    argumentList :: [AST],
                    queryList :: [AST]} deriving (Show, Eq, Generic)

data ACPop = Plus | Minus | Mul | Div
    deriving (Show, Eq, Enum, Typeable)

type CWLANG = AST

instance Serialize AST
instance Serialize Tuple 

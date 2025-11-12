{-# LANGUAGE DerivingStrategies #-}
module KDL.Types
  ( Parser
  , Document(..)
  , Content(..)
  , Node(..)
  , Value(..)
  , ValueType(..)
  , Identifier(..)
  ) where

import           Data.Map                       ( Map )
import           Data.Scientific                ( Scientific )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec )

{-
  String has exactly one use, 
  and that’s showing Hello World in tutorials.
                  -- Albert Einstein
-}
type Parser = Parsec Void Text

newtype Document = Document
  { docNodes :: [Node]
  }
  deriving (Eq, Show)

{- 
  This data type serves as an abstraction over Values
  and Properties of a Node, in order simplify the the node
  Parser, i.e group the two types together to
  consume any number of them in any order. 
-}
data Content
  = NodeValue    { getValue :: Value }
  | NodeProperty { getProp :: (Identifier, Value) }
  deriving (Eq, Show)

data Node = Node
  { nodeAnn      :: Maybe Identifier
  , nodeName     :: Identifier
  , nodeArgs     :: [Value]
  , nodeProps    :: Map Identifier Value
  , nodeChildren :: [Node]
  }
  deriving (Show, Eq)

newtype Identifier = Identifier Text
  deriving (Show, Eq)

data Value = Value
  { valueAnn :: Maybe Identifier
  , valueExp :: ValueType
  }
  deriving (Show, Eq)

data ValueType
  = StringValue Text
  | IntegerValue Integer
  | SciValue Scientific
  | BooleanValue Bool
  | NullValue
  deriving (Show, Eq)

{- 
  This allows for querying properties in alphabetical order
  upon printing out, the rest is handled automatically by Map.
-}
instance Ord Identifier where
  Identifier t1 `compare` Identifier t2 = t1 `compare` t2

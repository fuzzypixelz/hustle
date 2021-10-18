module KDL.Types
  ( Parser
  , Document(..)
  , Content(..)
  , TypeAnnotation(..)
  , Node(..)
  , NodeTerminator(..)
  , Value(..)
  , ValueType(..)
  , Property(..)
  , Name(..)
  ) where

import           Data.List                      ( intercalate )
import           Data.Scientific                ( Scientific )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec )

type Parser = Parsec Void Text

newtype Document = Document { docNodes :: [Node] }
  deriving (Eq)

instance Show Document where
  show d = intercalate "" (map show (docNodes d))

-- This data type serves as an abstraction over Values
-- and Properties of a Node to simplify the the node
-- Parser, i.e group the two types together in order to
-- consume any number of them in any order.
data Content
  = NodeValue { getValue :: Value }
  | NodeProperty { getProp :: Property }
  deriving (Show, Eq)

type TypeAnnotation = Maybe Name

showAnn :: TypeAnnotation -> String
showAnn a = case a of
  Nothing -> ""
  Just n  -> "(" ++ show n ++ ")"

data Node = Node
  { nodeAnn        :: TypeAnnotation
  , nodeName       :: Name
  , nodeArgs       :: [Value]
  , nodeProps      :: [Property]
  , nodeChildren   :: [Node]
  , nodeTerminator :: NodeTerminator
  }
  deriving Eq

instance Show Node where
  show n =
    ( unwords
      . filter (/= "")
      $ [ showAnn (nodeAnn n) ++ show (nodeName n)
        , unwords (map show (nodeArgs n))
        , unwords (map show (nodeProps n))
        , unwords (map show (nodeChildren n))
        ]
      )
      ++ show (nodeTerminator n)

data NodeTerminator
  = Semicolon
  | Newline
  | LineComment
  | EOF
  deriving (Eq)

instance Show NodeTerminator where
  show t = case t of
    Semicolon   -> "\n"
    Newline     -> "\n"
    LineComment -> ""
    EOF         -> "\n" -- TODO: verify if this should be ""

data ValueType
  = StringValue Text
  | RawStringValue Text -- ???
  | IntegerValue Integer
  | SciValue Scientific
  | HexValue Integer
  | OctalValue Integer
  | BinaryValue Integer
  | BooleanValue Bool
  | NullValue
  deriving (Eq)

-- TODO: use prettyprinter/simpleprint
instance Show ValueType where
  show (StringValue    t) = "\"" ++ T.unpack t ++ "\""
  show (RawStringValue r) = show r
    where hs = replicate (length $ filter (== '"') (T.unpack r)) '#'
  show (HexValue     h) = show h --"0x" ++ showHex h ""
  show (OctalValue   h) = show h --"0o" ++ showOct h ""
  show (BinaryValue  h) = show h --"0b" ++ showIntAtBase 2 intToDigit h ""
  show (BooleanValue b) = if b then "true" else "false"
  show (IntegerValue d) = show d
  show (SciValue     d) = map (\c -> if c == 'e' then 'E' else c) $ show d
  show NullValue        = "null"

data Value = Value
  { valueAnn :: TypeAnnotation
  , valueExp :: ValueType
  }
  deriving Eq

instance Show Value where
  show v = showAnn (valueAnn v) ++ show (valueExp v)

data Property = Property
  { propKey   :: Name
  , propValue :: Value
  }
  deriving Eq

instance Show Property where
  show p = show (propKey p) ++ "=" ++ show (propValue p)

data Name
  = Identifier Text
  | QuotedString Text
  deriving (Eq)

instance Show Name where
  show n = case n of
    Identifier   t -> T.unpack t
    QuotedString t -> show t

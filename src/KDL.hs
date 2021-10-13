{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module KDL where
  -- ( parseKDL
  -- , parseKDL'
  -- , Document(..)
  -- , Content(..)
  -- , Node(..)
  -- , Value(..)
  -- , ValueType(..)
  -- , Property(..)
  -- , Name(..)
  -- , NodeTerminator(..)
  -- ) where

import           Control.Monad
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Void
import           Data.List
import qualified Data.Text                     as T
import           Data.Scientific (Scientific, floatingOrInteger)
import qualified Data.Scientific               as Sci
import           Text.Megaparsec               hiding ( State )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import Numeric
import Data.Char (intToDigit, digitToInt, isDigit, toLower, isOctDigit, isHexDigit, isSpace)

type Parser = Parsec Void Text

newtype Document = Document { docNodes :: [Node] }
  deriving (Eq)

instance Show Document where
  show d = intercalate "" (map show (docNodes d))

data Content
  = NodeChild { getChild :: Maybe Node }
  | NodeValue { getValue :: Value }
  | NodeProperty { getProp :: Property }
  | Comment
  deriving (Show, Eq)

type TypeAnnotation = Maybe Name

showAnn :: TypeAnnotation -> String
showAnn a = case a of
  Nothing -> ""
  Just n -> "(" ++ show n ++ ")"

data Node = Node
  { nodeAnn      :: TypeAnnotation
  , nodeName     :: Name
  , nodeArgs     :: [Value]
  , nodeProps    :: [Property]
  , nodeChildren :: [Node]
  , nodeTerminator :: NodeTerminator
  , nodeIsChild    :: Bool }
  deriving (Eq)

instance Show Node where
  show n
    | nodeIsChild n = "{\n    " ++ nodeBody ++ "}" -- TODO: this shouldn't
    | otherwise     = nodeBody                   -- be hardcoded!    
    where
      nodeBody = (unwords . filter (/= "") $
        [ showAnn (nodeAnn n) ++ show (nodeName n)
        , unwords (map show (nodeArgs n))
        , unwords (map show (nodeProps n))
        , unwords (map show (nodeChildren n)) ])
        ++ show (nodeTerminator n)

data NodeTerminator
  = Semicolon
  | Newline
  | EOF
  deriving (Eq)

instance Show NodeTerminator where
  show t = case t of
    Semicolon -> ";"
    Newline -> "\n"
    EOF -> "\n" -- TODO: verify if this should be ""

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
  show (StringValue t) = "\"" ++ T.unpack t ++ "\""
  show (RawStringValue r) = "r" ++ hs ++ show r ++ hs
    where hs = replicate (length $ filter (== '"') (T.unpack r)) '#'
  show (HexValue h) = show h --"0x" ++ showHex h ""
  show (OctalValue h) = show h --"0o" ++ showOct h ""
  show (BinaryValue h) = show h --"0b" ++ showIntAtBase 2 intToDigit h ""
  show (BooleanValue b) = if b then "true" else "false"
  show (IntegerValue d) = show d
  show (SciValue d) = map (\c -> if c == 'e' then 'E' else c) $ show d
    -- Right i -> show i
  show NullValue = "null"

data Value = Value
  { valueAnn :: TypeAnnotation
  , value :: ValueType }
  deriving (Eq)

instance Show Value where
  show v = showAnn (valueAnn v) ++ show (value v)

data Property = Property
  { propKey   :: Name
  , propValue :: Value }
  deriving (Eq)

instance Show Property where
  show p = show (propKey p) ++ "=" ++ show (propValue p)

data Name
  = Identifier Text
  | QuotedString Text
  deriving (Eq)

instance Show Name where
  show n = case n of
    Identifier t -> T.unpack t
    QuotedString t -> show t

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

linebreak :: Parser ()
linebreak = choice $ map void
  [ char '\r' <?> "carriage return"
  , newline
  , char '\x85' <?> "next line"
  , char '\f' <?> "form feed"
  , char '\x2028' <?> "line seperator"
  , char '\x2029' <?> "paragraph seperator" ]
  ++ [ void crlf ]

hspaceChar :: Parser Char
hspaceChar = satisfy (\c -> isSpace c && c /= '\n')

ws :: Parser ()
ws = void (char '\xFEFF') <|> void hspaceChar <|> blockComment

escline :: Parser ()
escline = void $ char '\\' >> many ws >> (void newline <|> lineComment)

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

hsc :: Parser ()
hsc = L.space
  hspace1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

hlexeme :: Parser a -> Parser a
hlexeme = L.lexeme hsc
-- (.:) f g x y = f (g x y)
(.:) = (.) . (.); infixr 7 .:

manyTill__ :: Parser a -> Parser a -> Parser [a]
manyTill__ = fmap addEnding .: manyTill_
  where addEnding (xs, x) = xs ++ [x]

character :: Parser String
character = do
  void (char '\\')
  e <- escape
  return (if e == '\\' then [e] else ['\\', e])
  <|> (:[]) <$> noneOf ("\\\"" :: [Char])

escape :: Parser Char
escape = oneOf ("\"\\/bfnrt" :: [Char]) <|> (string "u{" *> count' 1 6 hexDigitChar *> char '}')

number :: Integer -> (Char -> Bool) -> Parser Integer
number b isNumDigit = mkNum . T.filter (/= '_') <$> digits
  where
    mkNum = T.foldl' step 0
    step a c = a * b + fromIntegral (digitToInt c)
    digits = T.cons
      <$> satisfy isNumDigit
      <*> takeWhileP (Just "digit") (\c -> isNumDigit c || c == '_')

isBinDigit :: Char -> Bool
isBinDigit c = c `elem` ['0', '1']

binary :: Parser Integer
binary = hlexeme $ L.signed hsc $ char '0' >> char' 'b' >> number 2 isBinDigit

octal :: Parser Integer
octal = hlexeme $ L.signed hsc $ char '0' >> char 'o' >> number 8 isOctDigit

hexadecimal :: Parser Integer
hexadecimal = hlexeme $ L.signed hsc $ char '0' >> char 'x' >> number 16 isHexDigit

integer :: Parser Integer
integer = hlexeme $ L.signed hsc decimal_

scientific :: Parser Scientific
scientific = hlexeme $ L.signed hsc scientific_

data SP = SP Integer Int

decimal_ :: Parser Integer
decimal_ = number 10 isDigit

scientific_ :: Parser Sci.Scientific
scientific_ = do
  c' <- decimal_
  -- SP c e' <- option (SP c' 0) (try $ dotDecimal_ c')
  SP c e' <- dotDecimal_ c'
  e <- option e' (try $ exponent_ e')
  return (Sci.scientific c e)

dotDecimal_ :: Integer -> Parser SP
dotDecimal_ c' = do
  void (char '.')
  let digits = T.cons
        <$> digitChar
        <*> takeWhileP (Just "digit") (\c -> isDigit c || c == '_')
  let mkNum = T.foldl' step (SP c' 0)
      step (SP a e') c =
        SP
          (a * 10 + fromIntegral (digitToInt c))
          (e' - 1)
  mkNum . T.filter (/= '_') <$> digits

exponent_ :: Int -> Parser Int
exponent_ e' = do
  void (char' 'e')
  (+ e') <$> L.signed (return ()) (fromIntegral <$> decimal_)

pIdentifier :: Parser Text
pIdentifier = label "Identifier" $ do
  i  <- satisfy initial
  is <- many (satisfy identifier)
  let result = T.pack (i : is)
  case result of
    "true" -> fail "keyword true in identifier"
    "false" -> fail "keyword false in identifier"
    _ -> return result
 where
  identifier c =
    c > '\x20' && c <= '\x10FFFF' && c `notElem` ("\\/(){}<>;[]=,\"" :: [Char])
  initial c = identifier c && c `notElem` ['0' .. '9']

pString :: Parser Text
pString = hlexeme $ do
  T.pack . concat <$> (char '"' *> manyTill character (char '"'))

pName :: Parser Name
pName = choice
  [ Identifier <$> try pIdentifier
  , QuotedString <$> pString <?> "quoted String" ]

pTypeAnnotation :: Parser Name
pTypeAnnotation = label "Type Annotation" $ do
  void (char '(')
  i <- pName
  void (char ')')
  return i

pValue :: Parser Value
pValue = hlexeme $ do
  valueAnn <- optional pTypeAnnotation
  value <- choice
    [ BinaryValue <$> try binary <?> "Bin"
    , OctalValue <$> try octal <?> "Oct"
    , HexValue <$> try hexadecimal <?> "Hex"
    , SciValue <$> try scientific <?> "Float"
    , IntegerValue <$> try integer <?> "Integer"
    , BooleanValue <$> try pBool <?> "Boolean"
    , NullValue <$ try pNull <?> "Null"
    , StringValue <$> try pString <?> "String"
    , RawStringValue . T.pack <$> try pRawString <?> "Raw String" ]
  return Value {..}
 where
  pNull = hlexeme (string "null")
  pBool   = hlexeme (True <$ string "true" <|> False <$ string "false")
  pRawString = hlexeme $ do
    void (char 'r')
    hs <- many (char '#')
    void (char '"')
    let h = length hs
    s <- count (h + 1) (manyTill__ anySingle (char '"'))
    void (count h (char '#'))
    return (init . concat $ s)

pProperty :: Parser Property
pProperty = hlexeme $ do
  propKey <- pName
  void (char '=')
  propValue <- pValue
  return Property { .. }

pChild :: Parser (Maybe Node)
pChild = hlexeme $ do
  void (char '{' >> sc)
  child <- optional $ pNode True
  void (sc >> char '}')
  return child

pComment :: Parser ()
pComment = do
  void $ hlexeme (string "/-")
  choice
    [ void $ try pChild
    , void $ try pProperty
    , void $ try pValue ]

pContent :: Parser Content
pContent = hlexeme $ choice
  [ Comment <$ try pComment
  , NodeChild <$> try pChild
  , NodeProperty <$> try pProperty
  , NodeValue <$> try pValue ]

pNode :: Bool -> Parser Node
pNode nodeIsChild = lexeme $ do
  nodeAnn <- optional pTypeAnnotation
  nodeName <- hlexeme pName
  content  <- many pContent
  nodeTerminator <- choice
    [ Semicolon <$ try (char ';')
    , Newline <$ try newline
    , EOF <$ eof ]
  -- The Comment values are automagically skipped!
  let nodeChildren = mapMaybe isChild content
      nodeArgs     = mapMaybe isArg content
      nodeProps    = mapMaybe isProp content
  return Node { .. }
 where
  isChild c = case c of
    NodeChild (Just n) -> Just n
    _           -> Nothing
  isArg c = case c of
    NodeValue v -> Just v
    _           -> Nothing
  isProp c = case c of
    NodeProperty p -> Just p
    _              -> Nothing

pDocument :: Parser Document
pDocument = do
  void sc
  docNodes_ <- many $ do
    c <- optional (string "/-" >> sc)
    case c of
      Just _ -> do
        void $ pNode False
        return Nothing
      Nothing -> Just <$> pNode False
  void (sc >> eof)
  let docNodes = catMaybes docNodes_
  return Document { .. }


parseKDL' :: IO ()
parseKDL' = do
  putStrLn "Type a valid KDL document:"
  doc <- T.pack <$> getLine
  parseTest pDocument doc

parseKDL :: String -> Document
parseKDL s = do
  let doc = T.pack s
  case runParser pDocument "" doc of
    Right d -> d
    Left  _ -> Document { docNodes = [] }


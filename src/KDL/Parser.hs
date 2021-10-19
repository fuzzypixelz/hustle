{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module KDL.Parser where

import           KDL.Internal
import           KDL.Types

import qualified Control.Concurrent.QSem       as T
import           Control.Monad                  ( void )
import           Data.Char                      ( chr
                                                , isHexDigit
                                                , isOctDigit
                                                , isSpace
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , mapMaybe
                                                , maybeToList
                                                )
import           Data.Scientific                ( Scientific )
import qualified Data.Scientific               as Sci
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Numeric                        ( readHex )
import           Text.Megaparsec                ( (<?>)
                                                , (<|>)
                                                , MonadParsec(eof, label, try)
                                                , anySingle
                                                , between
                                                , choice
                                                , count
                                                , count'
                                                , many
                                                , manyTill
                                                , noneOf
                                                , oneOf
                                                , optional
                                                , parseTest
                                                , runParser
                                                , satisfy
                                                , some
                                                , someTill
                                                )
import           Text.Megaparsec.Char           ( char
                                                , char'
                                                , crlf
                                                , hexDigitChar
                                                , newline
                                                , string
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Megaparsec.Debug

-- WHITESPACE

escline :: Parser ()
escline = void (char '\\') >> many ws >> (try linebreak <|> lineComment)

linespace :: Parser ()
linespace = try linebreak <|> try ws <|> try lineComment <?> "Line Space"

linebreak :: Parser ()
linebreak =
  choice
    $  map
         void
         [ char '\r' <?> "carriage return"
         , newline
         , char '\x85' <?> "next line"
         , char '\f' <?> "form feed"
         , char '\x2028' <?> "line seperator"
         , char '\x2029' <?> "paragraph seperator"
         ]
    ++ [void crlf]

ws :: Parser ()
ws = bom <|> void hspacechar <|> blockComment <?> "Whitespace"

bom :: Parser ()
bom = void $ char '\xFEFF'

hspacechar :: Parser Char
hspacechar = satisfy (\c -> isSpace c && c /= '\n')

-- STRINGS

anystring :: Parser Text
anystring = try rawstring <|> escstring

escstring :: Parser Text
escstring = do
  T.concat <$> (char '"' *> manyTill character (char '"'))

rawstring :: Parser Text
rawstring = do
  void (char 'r')
  hs <- many (char '#')
  void (char '"')
  let h = length hs
  s <- count (h + 1) (manyTill__ anySingle (char '"'))
  void (count h (char '#'))
  return . T.pack $ (init . concat $ s)

character :: Parser Text
character =
  void (char '\\') *> escape <|> T.singleton <$> noneOf ("\\\"" :: [Char])

escape :: Parser Text
escape =
  do
    c <- oneOf ("\"\\/bfnrt" :: [Char])
    case c of
      '\\' -> return (T.singleton c)
      e    -> return (T.pack ['\\', e])
  <|> uescape

uescape :: Parser Text
uescape = do
  void (string "u{")
  u <- fromInteger <$> number 16 isHexDigit
  if u >= 0x10ffff
    then fail "Exceeded Unicode code point limit."
    else do
      void (char '}')
      let s = chr u
      return (T.singleton s)

-- NUMBERS

binary :: Parser Integer
binary = L.signed hsc $ char '0' >> char' 'b' >> number 2 isBinDigit

octal :: Parser Integer
octal = L.signed hsc $ char '0' >> char 'o' >> number 8 isOctDigit

hexadecimal :: Parser Integer
hexadecimal = L.signed hsc $ char '0' >> char 'x' >> number 16 isHexDigit

integer :: Parser Integer
integer = L.signed hsc decimal_

scientific :: Parser Scientific
scientific = L.signed hsc scientific_

-- CONTENT

name :: Parser Name
name = choice [Identifier <$> try identifier, QuotedString <$> anystring]

identifier :: Parser Text
identifier = label "Identifier" $ do
  i  <- satisfy iichar
  is <- many (satisfy ichar)
  let result = T.pack (i : is)
  case result of
    "true"  -> fail "keyword true in identifier"
    "false" -> fail "keyword false in identifier"
    "null"  -> fail "keyword null in identifier"
    _       -> return result
 where
  ichar c =
    c > '\x20' && c <= '\x10FFFF' && c `notElem` ("\\/(){}<>;[]=,\"" :: [Char])
  iichar c = ichar c && c `notElem` ['0' .. '9']

nullvalue :: Parser Text
nullvalue = string "null"

bool :: Parser Bool
bool = True <$ string "true" <|> False <$ string "false"

property :: Parser Property
property = label "Property" $ do
  propKey <- name
  void (char '=')
  propValue <- value
  return Property { .. }

value :: Parser Value
value = label "Value" $ do
  valueAnn <- optional typeAnnotation
  valueExp <- choice
    [ BinaryValue <$> try binary <?> "Bin"
    , OctalValue <$> try octal <?> "Oct"
    , HexValue <$> try hexadecimal <?> "Hex"
    , SciValue <$> try scientific <?> "Float"
    , IntegerValue <$> try integer <?> "Integer"
    , BooleanValue <$> try bool <?> "Boolean"
    , NullValue <$ try nullvalue <?> "Null"
    , StringValue <$> try escstring <?> "String"
    , RawStringValue <$> try rawstring <?> "Raw String"
    ]
  return Value { .. }

typeAnnotation :: Parser Name
typeAnnotation = label "Type Annotation" $ do
  void (char '(')
  i <- name
  void (char ')')
  return i

-- NODES

nodes :: Parser [Node]
nodes = between (many linespace) (many linespace) (fromMaybe [] <$> body)
 where
  body = optional $ do
    n  <- maybeToList <$> node
    ns <- fromMaybe [] <$> optional nodes
    return (n ++ ns)

node :: Parser (Maybe Node)
node = do
  discard        <- optional comment
  nodeAnn        <- optional typeAnnotation
  nodeName       <- name
  nodeContent    <- catMaybes <$> content
  nodeChildren   <- fromMaybe [] <$> optional children
  _              <- many nodespace
  nodeTerminator <- terminator
  let nodeArgs  = mapMaybe isArg nodeContent
      nodeProps = mapMaybe isProp nodeContent
  case discard of
    Just _  -> return Nothing
    Nothing -> return $ Just Node { .. }
 where
  isArg c = case c of
    NodeValue v -> Just v
    _           -> Nothing
  isProp c = case c of
    NodeProperty p -> Just p
    _              -> Nothing

content :: Parser [Maybe Content]
content = many . try $ do
  void (some nodespace)
  discard <- optional comment
  void (many nodespace)
  c <- choice [NodeProperty <$> try property, NodeValue <$> try value]
  case discard of
    Just _  -> return Nothing
    Nothing -> return $ Just c

children :: Parser [Node]
children = try $ do
  void (many nodespace)
  discard <- optional comment
  void (char '{')
  ns <- nodes
  void (char '}')
  void (many ws)
  case discard of
    Just _  -> return []
    Nothing -> return ns

comment :: Parser ()
comment = try $ do
  void (string "/-")
  void $ many nodespace

nodespace :: Parser ()
nodespace = label "Node Space" $ do
  try (many ws *> escline <* many ws) <|> try (void $ some ws)

terminator :: Parser NodeTerminator
terminator = choice
  [ Semicolon <$ try (char ';')
  , Newline <$ try newline
  , LineComment <$ try lineComment
  , EOF <$ eof
  ]

pDocument :: Parser Document
pDocument = do
  docNodes <- nodes
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

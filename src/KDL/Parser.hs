{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module KDL.Parser where

import           KDL.Internal
import           KDL.Types

import           Control.Monad                  ( void )
import           Data.Char                      ( chr
                                                , isHexDigit
                                                , isOctDigit
                                                , isSpace
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , mapMaybe
                                                , maybeToList
                                                )
import           Data.Scientific                ( Scientific )
import qualified Data.Scientific               as Sci
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
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
escline =
  char '\\' >> many ws >> (try linebreak <|> lineComment) <?> "Escape Line"

linespace :: Parser ()
linespace = try linebreak <|> try ws <|> try lineComment <?> "Line Space"

linebreak :: Parser ()
linebreak = label "Newline" $ do
  choice $ void crlf : map
    void
    [ char '\r' <?> "carriage return"
    , char '\n' <?> "newline"
    , char '\x85' <?> "next line"
    , char '\f' <?> "form feed"
    , char '\x2028' <?> "line seperator"
    , char '\x2029' <?> "paragraph seperator"
    ]

ws :: Parser ()
ws = bom <|> hspacechar <|> blockComment <?> "Whitespace"

bom :: Parser ()
bom = void (char '\xFEFF') <?> "BOM"

hspacechar :: Parser ()
hspacechar = label "Unicode Space" $ do
  choice $ map
    void
    [ char '\x0009' <?> "character tabulation"
    , char '\x0020' <?> "space"
    , char '\x00A0' <?> "bo-break space"
    , char '\x1680' <?> "ogham space mark"
    , char '\x2000' <?> "en quad"
    , char '\x2001' <?> "em quad"
    , char '\x2002' <?> "en space"
    , char '\x2003' <?> "em space"
    , char '\x2004' <?> "three-per-em space"
    , char '\x2005' <?> "four-per-em space"
    , char '\x2006' <?> "six-per-em space"
    , char '\x2007' <?> "figure space"
    , char '\x2008' <?> "punctuation space"
    , char '\x2009' <?> "thin space"
    , char '\x200A' <?> "hair space"
    , char '\x202F' <?> "narrow no-break space"
    , char '\x205F' <?> "medium mathmatical space"
    , char '\x3000' <?> "ideographic space"
    ]

-- STRINGS

anystring :: Parser Text
anystring = try rawstring <|> escstring

escstring :: Parser Text
escstring = label "String" $ do
  T.concat <$> (char '"' *> manyTill character (char '"'))

rawstring :: Parser Text
rawstring = label "Raw String" $ do
  void (char 'r')
  hs <- T.pack <$> many (char '#')
  void (char '"')
  s <- manyTill anySingle (string (T.cons '"' hs))
  return (T.pack s)

character :: Parser Text
character = void (char '\\') *> escape <|> nonescape

{-
  As per the Haskell 2010 Language Report,
  (https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6)
  The '\/' Solidus escape isn't defined, so if we try to parse it directly the
  compiler throws a lexical error. Hence this terribleness.
-}
escape :: Parser Text
escape =
  do
    e <- choice
      [ '\x08' <$ char 'b'
      , '\x09' <$ char 't'
      , '\x0A' <$ char 'n'
      , '\x0C' <$ char 'f'
      , '\x0D' <$ char 'r'
      , '\x22' <$ char '\"'
      , '\x2F' <$ char '/'
      , '\x5C' <$ char '\\'
      ]
    return (T.singleton e)
  <|> uescape

uescape :: Parser Text
uescape = do
  void (string "u{")
  u <- fromInteger <$> number 16 isHexDigit
  if u >= 0x10ffff
    then fail "Exceeded Unicode code point limit."
    else do
      void (char '}')
      let c = chr u
      return (T.singleton c)

nonescape :: Parser Text
nonescape = do
  c <- noneOf ("\\\"" :: [Char])
  return (T.singleton c)

-- NUMBERS

binary :: Parser Integer
binary = signed $ char '0' >> char' 'b' >> number 2 isBinDigit

octal :: Parser Integer
octal = signed $ char '0' >> char 'o' >> number 8 isOctDigit

hexadecimal :: Parser Integer
hexadecimal = signed $ char '0' >> char 'x' >> number 16 isHexDigit

integer :: Parser Integer
integer = signed decimal_

scientific :: Parser Scientific
scientific = signed scientific_

-- CONTENT

name :: Parser Identifier
name = Identifier <$> (try anystring <|> identifier)

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

property :: Parser (Identifier, Value)
property = label "Property" $ do
  propKey <- name
  void (char '=')
  propValue <- value
  return (propKey, propValue)

value :: Parser Value
value = label "Value" $ do
  valueAnn <- optional typeAnnotation
  valueExp <- choice
    [ IntegerValue <$> try binary <?> "Binary"
    , IntegerValue <$> try octal <?> "Octal"
    , IntegerValue <$> try hexadecimal <?> "Hexadecimal"
    , SciValue <$> try scientific <?> "Decimal"
    , BooleanValue <$> try bool <?> "Boolean"
    , NullValue <$ try nullvalue <?> "Null"
    , StringValue <$> anystring <?> "String"
    ]
  return Value { .. }

typeAnnotation :: Parser Identifier
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
node = label "Node" $ do
  discard      <- optional comment
  nodeAnn      <- optional typeAnnotation
  nodeName     <- name
  nodeContent  <- catMaybes <$> content
  nodeChildren <- fromMaybe [] <$> optional children
  _            <- many nodespace
  _            <- terminator
  let nodeArgs  = mapMaybe isArg nodeContent
      nodeProps = Map.fromList $ mapMaybe isProp nodeContent
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
  discard <- optional $ comment <* many nodespace
  c       <- choice [NodeProperty <$> try property, NodeValue <$> try value]
  case discard of
    Just _  -> return Nothing
    Nothing -> return $ Just c

children :: Parser [Node]
children = label "Node Child" . try $ do
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
comment = label "/-Comment" . try $ do
  void (string "/-")
  void $ many nodespace

nodespace :: Parser ()
nodespace = label "Node Space" $ do
  try (many ws *> escline <* many ws) <|> try (void $ some ws)

terminator :: Parser ()
terminator = label "Node Terminator" $ do
  choice [try (void (char ';')), try (void newline), try lineComment, eof]

document :: Parser Document
document = do
  docNodes <- nodes
  void eof
  return Document { .. }

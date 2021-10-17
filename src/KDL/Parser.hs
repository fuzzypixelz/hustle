{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module KDL.Parser where

import           KDL.Internal
import           KDL.Types

import           Control.Monad                  ( void )
import           Data.Char                      ( isHexDigit
                                                , isOctDigit
                                                , isSpace
                                                )
import           Data.Maybe                     ( catMaybes
                                                , mapMaybe
                                                )
import           Data.Scientific                ( Scientific )
import qualified Data.Scientific               as Sci
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Megaparsec                ( (<?>)
                                                , (<|>)
                                                , MonadParsec(eof, label, try)
                                                , anySingle
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
                                                )
import           Text.Megaparsec.Char           ( char
                                                , char'
                                                , crlf
                                                , hexDigitChar
                                                , newline
                                                , string
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L

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

hspaceChar :: Parser Char
hspaceChar = satisfy (\c -> isSpace c && c /= '\n')

ws :: Parser ()
ws = void (char '\xFEFF') <|> void hspaceChar <|> blockComment

escline :: Parser ()
escline = void $ char '\\' >> many ws >> (void newline <|> lineComment)

character :: Parser String
character =
  do
      void (char '\\')
      e <- escape
      return (if e == '\\' then [e] else ['\\', e])
    <|> (: [])
    <$> noneOf ("\\\"" :: [Char])

escape :: Parser Char
escape =
  oneOf ("\"\\/bfnrt" :: [Char])
    <|> (string "u{" *> count' 1 6 hexDigitChar *> char '}')


binary :: Parser Integer
binary = hlexeme $ L.signed hsc $ char '0' >> char' 'b' >> number 2 isBinDigit

octal :: Parser Integer
octal = hlexeme $ L.signed hsc $ char '0' >> char 'o' >> number 8 isOctDigit

hexadecimal :: Parser Integer
hexadecimal =
  hlexeme $ L.signed hsc $ char '0' >> char 'x' >> number 16 isHexDigit

integer :: Parser Integer
integer = hlexeme $ L.signed hsc decimal_

scientific :: Parser Scientific
scientific = hlexeme $ L.signed hsc scientific_

pIdentifier :: Parser Text
pIdentifier = label "Identifier" $ do
  i  <- satisfy initial
  is <- many (satisfy identifier)
  let result = T.pack (i : is)
  case result of
    "true"  -> fail "keyword true in identifier"
    "false" -> fail "keyword false in identifier"
    _       -> return result
 where
  identifier c =
    c > '\x20' && c <= '\x10FFFF' && c `notElem` ("\\/(){}<>;[]=,\"" :: [Char])
  initial c = identifier c && c `notElem` ['0' .. '9']

pString :: Parser Text
pString = hlexeme $ do
  T.pack . concat <$> (char '"' *> manyTill character (char '"'))

pName :: Parser Name
pName = choice
  [Identifier <$> try pIdentifier, QuotedString <$> pString <?> "quoted String"]

pTypeAnnotation :: Parser Name
pTypeAnnotation = label "Type Annotation" $ do
  void (char '(')
  i <- pName
  void (char ')')
  return i

pValue :: Parser Value
pValue = hlexeme $ do
  valueAnn <- optional pTypeAnnotation
  value    <- choice
    [ BinaryValue <$> try binary <?> "Bin"
    , OctalValue <$> try octal <?> "Oct"
    , HexValue <$> try hexadecimal <?> "Hex"
    , SciValue <$> try scientific <?> "Float"
    , IntegerValue <$> try integer <?> "Integer"
    , BooleanValue <$> try pBool <?> "Boolean"
    , NullValue <$ try pNull <?> "Null"
    , StringValue <$> try pString <?> "String"
    , RawStringValue . T.pack <$> try pRawString <?> "Raw String"
    ]
  return Value { .. }
 where
  pNull      = hlexeme (string "null")
  pBool      = hlexeme (True <$ string "true" <|> False <$ string "false")
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
  choice [void $ try pChild, void $ try pProperty, void $ try pValue]

pContent :: Parser Content
pContent = hlexeme $ choice
  [ Comment <$ try pComment
  , NodeChild <$> try pChild
  , NodeProperty <$> try pProperty
  , NodeValue <$> try pValue
  ]

pNode :: Bool -> Parser Node
pNode nodeIsChild = lexeme $ do
  nodeAnn        <- optional pTypeAnnotation
  nodeName       <- hlexeme pName
  content        <- many pContent
  nodeTerminator <- choice
    [Semicolon <$ try (char ';'), Newline <$ try newline, EOF <$ eof]
  -- The Comment values are automagically skipped!
  let nodeChildren = mapMaybe isChild content
      nodeArgs     = mapMaybe isArg content
      nodeProps    = mapMaybe isProp content
  return Node { .. }
 where
  isChild c = case c of
    NodeChild (Just n) -> Just n
    _                  -> Nothing
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

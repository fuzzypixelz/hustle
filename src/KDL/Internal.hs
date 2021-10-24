{-# LANGUAGE OverloadedStrings #-}

module KDL.Internal where

import           Control.Monad                  ( void )
import           Data.Char                      ( digitToInt
                                                , isDigit
                                                )
import           Data.Either                    ( isRight )
import           Data.Scientific                ( Scientific )
import qualified Data.Scientific               as Sci
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           KDL.Types                      ( Parser )
import           Text.Megaparsec                ( (<|>)
                                                , MonadParsec
                                                  ( eof
                                                  , takeWhileP
                                                  , try
                                                  )
                                                , manyTill_
                                                , option
                                                , runParser
                                                , satisfy
                                                )
import           Text.Megaparsec.Char           ( char
                                                , char'
                                                , digitChar
                                                , newline
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L

signed :: Num a => Parser a -> Parser a
signed p = option id sign <*> p
  where sign = (id <$ char '+') <|> (negate <$ char '-')

lineComment :: Parser ()
lineComment = L.skipLineComment "//" >> (void newline <|> eof)

blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "/*" "*/"

isBinDigit :: Char -> Bool
isBinDigit c = c `elem` ['0', '1']

data SP = SP Integer Int

number :: Integer -> (Char -> Bool) -> Parser Integer
number b isNumDigit = mkNum . T.filter (/= '_') <$> digits
 where
  mkNum = T.foldl' step 0
  step a c = a * b + fromIntegral (digitToInt c)
  digits = T.cons <$> satisfy isNumDigit <*> takeWhileP
    (Just "digit")
    (\c -> isNumDigit c || c == '_')

decimal_ :: Parser Integer
decimal_ = number 10 isDigit

scientific_ :: Parser Scientific
scientific_ = do
  c'      <- decimal_
  SP c e' <- option (SP c' 0) (try $ dotDecimal_ c')
  e       <- option e' (try $ exponent_ e')
  return (Sci.scientific c e)

dotDecimal_ :: Integer -> Parser SP
dotDecimal_ c' = do
  void (char '.')
  let digits = T.cons <$> digitChar <*> takeWhileP
        (Just "digit")
        (\c -> isDigit c || c == '_')
  let mkNum = T.foldl' step (SP c' 0)
      step (SP a e') c = SP (a * 10 + fromIntegral (digitToInt c)) (e' - 1)
  mkNum . T.filter (/= '_') <$> digits

exponent_ :: Int -> Parser Int
exponent_ e' = do
  void (char' 'e')
  (+ e') <$> L.signed (return ()) (fromIntegral <$> decimal_)

match :: Parser a -> Text -> Bool
match p t = isRight $ runParser (p >> eof) "" t

escChar :: Char -> Text
escChar c = case c of
  '\x08' -> "\\b"
  '\x09' -> "\\t"
  '\x0A' -> "\\n"
  '\x0C' -> "\\f"
  '\x0D' -> "\\r"
  '\x22' -> "\\\""
  '\x2F' -> "\\/"
  '\x5C' -> "\\\\"
  c      -> T.singleton c

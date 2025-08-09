{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}

module KDL.Internal where

import Data.Functor
import           Data.Char                      ( digitToInt
                                                , isDigit
                                                )
import           Data.Either                    ( isRight )
import qualified Data.Scientific               as Sci
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           KDL.Types                      ( Parser, ValueType(..))
import           Text.Megaparsec                ( (<|>)
                                                , MonadParsec
                                                  ( eof
                                                  , takeWhileP
                                                  )
                                                , choice
                                                , option
                                                , runParser
                                                , satisfy
                                                )
import           Text.Megaparsec.Char           ( char
                                                , digitChar
                                                , newline
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L

lineComment :: Parser ()
lineComment = L.skipLineComment "//" >> (void newline <|> eof)

blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "/*" "*/"

isBinDigit :: Char -> Bool
isBinDigit c = c `elem` ['0', '1']

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

signed :: Num a => Parser a -> Parser a
signed p =
  option 
    id 
    ((char '+' $> id)
      <|> (char '-' $> negate))
  <*> p

scientific :: Parser ValueType
scientific = do
    sign <- signed (pure 1)
    whole <- decimal_
    choice
      [ char '.' *> do
          (mantissa, e0) <- fractionalPart whole
          exponent <- option e0 $ (char 'e' *> exponentPart e0) <|> pure e0
          pure $ SciValue $ fromInteger sign * Sci.scientific mantissa exponent
      , char 'e' *> do
          exponent <- exponentPart 0
          pure $ SciValue $ fromInteger sign * Sci.scientific whole exponent
      , do
          pure $ IntegerValue $ sign * whole
      ]
  where
    fractionalPart :: Integer -> Parser (Integer, Int)
    fractionalPart whole = do
      let digits = T.cons <$> digitChar <*> takeWhileP
            (Just "digit")
            (\c -> isDigit c || c == '_')
      let step (a, e) c = (a * 10 + fromIntegral (digitToInt c), e - 1)
      let mkNum = T.foldl' step (whole, 0)
      mkNum . T.filter (/= '_') <$> digits

    exponentPart :: Int -> Parser Int
    exponentPart e0 = do
      e1 :: Int <- L.signed (pure ()) (fromIntegral <$> decimal_)
      pure (e0 + e1)

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

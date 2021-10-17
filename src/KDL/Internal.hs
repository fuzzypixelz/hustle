{-# LANGUAGE OverloadedStrings #-}

module KDL.Internal 
where

import KDL.Types ( Parser )

import Control.Monad ( void )
import Data.Char ( isDigit, digitToInt )                      
import           Data.Scientific                ( Scientific )
import qualified Data.Scientific               as Sci
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import Text.Megaparsec
    ( satisfy, option, manyTill_, MonadParsec(takeWhileP, try) )
import Text.Megaparsec.Char
    ( char, char', digitChar, hspace1, space1 )
import qualified Text.Megaparsec.Char.Lexer    as L

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

sc :: Parser ()
sc = L.space
  space1
  lineComment
  blockComment

hsc :: Parser ()
hsc = L.space
  hspace1
  lineComment
  blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

hlexeme :: Parser a -> Parser a
hlexeme = L.lexeme hsc
-- (.:) f g x y = f (g x y)
(.:) = (.) . (.); infixr 7 .:

manyTill__ :: Parser a -> Parser a -> Parser [a]
manyTill__ = fmap addEnding .: manyTill_
  where addEnding (xs, x) = xs ++ [x]

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

scientific_ :: Parser Sci.Scientific
scientific_ = do
  c'      <- decimal_
  -- SP c e' <- option (SP c' 0) (try $ dotDecimal_ c')
  SP c e' <- dotDecimal_ c'
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
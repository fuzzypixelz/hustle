{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module KDL.Formatter
  ( Pretty(pretty)
  ) where

import           GHC.Stack
import           Data.Char                      (toUpper, intToDigit)
import           Data.Map                       ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( catMaybes )
import           Data.Scientific                ( Scientific )
import qualified Data.Scientific               as Scientific
import qualified Data.Text                     as T
import           KDL.Internal                   ( escChar
                                                , match
                                                )
import           KDL.Parser                     ( identifier )
import           KDL.Types
import           Prettyprinter                  ( Pretty(pretty)
                                                , braces
                                                , dquotes
                                                , enclose
                                                , hsep
                                                , nest
                                                , parens
                                                , viaShow
                                                , vsep
                                                )

instance Pretty Scientific where
  pretty s
      | Scientific.coefficient s < 0 = pretty $ T.pack $ "-" <> showPositive (-s)
      | otherwise = pretty $ T.pack $ showPositive s
    where
      showPositive :: Scientific -> String
      showPositive = fmtAsGeneric . Scientific.toDecimalDigits

      fmtAsGeneric :: ([Int], Int) -> String
      fmtAsGeneric x@(_is, e)
          | e < 0 || e > 7 = fmtAsExponent x
          | otherwise      = fmtAsFixed    x

fmtAsExponent :: HasCallStack => ([Int], Int) -> String
fmtAsExponent (is, e) =
    case ds of
      "0"     -> "0.0E0"
      [d]     -> d : '.' : '0' : 'E' : show_e'
      (d:ds') -> d : '.' : ds' ++ ('E' : show_e')
      []      -> error "Empty list of decimals"
  where
    show_e' 
      | e > 0 = "+" <> show (e-1)
      | otherwise = show (e-1)

    ds = map intToDigit is

fmtAsFixed :: ([Int], Int) -> String
fmtAsFixed (is, e)
    | e <= 0    = '0':'.':(replicate (-e) '0' ++ ds)
    | otherwise =
        let
           f 0 s    rs  = mk0 (reverse s) ++ '.':mk0 rs
           f n s    ""  = f (n-1) ('0':s) ""
           f n s (r:rs) = f (n-1) (r:s) rs
        in
           f e "" ds
  where
    mk0 "" = "0"
    mk0 ls = ls

    ds = map intToDigit is

instance Pretty Identifier where
  pretty (Identifier i) =
    if match identifier i then pretty i else dquotes (pretty i)

instance Pretty Value where
  pretty v = vann <> vexp
   where
    vann = case valueAnn v of
      Nothing -> ""
      Just a  -> parens (pretty a)
    vexp = case valueExp v of
      StringValue  s -> dquotes . pretty $ T.concatMap escChar s
      IntegerValue i -> pretty i
      SciValue s -> pretty s
      BooleanValue b -> if b then "true" else "false"
      NullValue      -> "null"

instance Pretty (Map Identifier Value) where
  pretty ps = hsep . Map.elems $ Map.mapWithKey prop ps
    where prop i v = pretty i <> "=" <> pretty v

instance Pretty Node where
  pretty n = hsep . catMaybes $ [nname, nargs, nprops, nchildren]
   where
    nann = case nodeAnn n of
      Nothing -> ""
      Just a  -> parens (pretty a)
    nname = Just $ nann <> pretty (nodeName n)
    nargs = case nodeArgs n of
      []  -> Nothing
      nas -> Just . hsep . map pretty $ nas
    nprops | nodeProps n == Map.empty = Nothing
           | otherwise                = Just (pretty (nodeProps n))
    nchildren = case nodeChildren n of
      [] -> Nothing
      ncs ->
        Just
          . nest 4
          . braces
          . enclose "\n" (nest (-4) "\n")
          . vsep
          . map pretty
          $ ncs

instance Pretty Document where
  pretty d = vsep (map pretty (docNodes d)) <> "\n"

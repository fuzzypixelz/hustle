{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module KDL.Formatter
  ( Pretty(pretty)
  ) where

import           Data.Map                       ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( catMaybes )
import           Data.Scientific                ( Scientific )
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
  pretty = viaShow

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
      SciValue     s -> pretty s
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

instance Show Document where
  show d = show (pretty d)

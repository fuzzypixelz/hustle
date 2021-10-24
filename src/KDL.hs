module KDL
  ( Parser
  , Document(..)
  , Node(..)
  , Value(..)
  , ValueType(..)
  , Identifier(..)
  , pretty
  , document
  , parse
  , errorBundlePretty
  ) where

import           KDL.Formatter                  ( Pretty(pretty) )
import           KDL.Parser                     ( document )
import           KDL.Types                      ( Document(..)
                                                , Identifier(..)
                                                , Node(..)
                                                , Parser
                                                , Value(..)
                                                , ValueType(..)
                                                )
import           Text.Megaparsec                ( errorBundlePretty
                                                , parse
                                                )

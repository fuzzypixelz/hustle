module Main where

import qualified Data.Text                     as T
import           KDL                            ( document
                                                , errorBundlePretty
                                                , parse
                                                )

main :: IO ()
main = do
  input <- T.pack <$> getContents
  case parse document "" input of
    Left  e -> putStrLn (errorBundlePretty e)
    Right d -> print d


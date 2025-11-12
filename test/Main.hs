{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad.IO.Class
import qualified Data.Text                     as T
import           KDL                            ( document
                                                , parse
                                                , Document
                                                )
import           System.Directory               ( doesFileExist
                                                , getDirectoryContents
                                                )
import           System.FilePath                ( (</>) )
import qualified Test.HUnit
import           Test.Hspec                     ( SpecWith
                                                , describe
                                                , hspec
                                                , it
                                                , parallel
                                                , runIO
                                                , shouldBe
                                                )
import           Test.QuickCheck                ( )
import           Prettyprinter
import           Text.Megaparsec.Error

testCase :: FilePath -> FilePath -> SpecWith ()
testCase input expected = do
  describe "KDL.parse" $ do
    it ("should satisfy " ++ input) $ do
      inputFile     <- T.pack <$> readFile input
      shouldSucceed <- doesFileExist expected
      expectedResult <- parseExpectedResult expected
      case parse document input inputFile of
        Left e -> do
          liftIO $ putStrLn $ errorBundlePretty e
          shouldSucceed `shouldBe` False
        Right d ->
          (show $ pretty d) `shouldBe` (show $ pretty expectedResult)

parseExpectedResult :: FilePath -> IO Document
parseExpectedResult expected = do
  expectedFile <- T.pack <$> readFile expected
  case parse document expected expectedFile of
    Left e -> do
      putStrLn $ errorBundlePretty e
      Test.HUnit.assertFailure $ "Could not parse: " <> expected
    Right d -> do
      expectedFile <- readFile expected
      pure d

inputDir :: FilePath
inputDir = "kdl/tests/test_cases/input"

expectedDir :: FilePath
expectedDir = "kdl/tests/test_cases/expected_kdl"

main :: IO ()
main = hspec $ do
  parallel $ do
    describe "should pass the kdl-org provided test cases" $ do
      files_ <- runIO $ getDirectoryContents inputDir
      let files         = filter (`notElem` [".", ".."]) files_
      let inputFiles    = map (inputDir </>) files
      let expectedFiles = map (expectedDir </>) files
      let testCases     = zipWith testCase inputFiles expectedFiles
      sequence_ testCases

{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception              ( evaluate )
import           KDL
import           Test.Hspec
import           Test.QuickCheck
import           System.Directory
import           System.FilePath

testCase :: FilePath -> FilePath -> SpecWith ()
testCase input expected = do
  describe "KDL.parseKDL" $ do
    it ("should satisfy " ++ input) $ do
      inputFile <- readFile input
      shouldSucceed <- doesFileExist expected
      if shouldSucceed
        then do
          expectedFile <- readFile expected
          show (parseKDL inputFile) `shouldBe` expectedFile
        else do
          parseKDL inputFile `shouldBe` Document { docNodes = [] }

whatever :: String -> String -> IO ()
whatever x y = do
  putStrLn x
  putStrLn y

getAbsDirectoryContents :: String -> IO [FilePath]
getAbsDirectoryContents dir = do
  contents <- getDirectoryContents dir
  return $ map (dir </>) contents

inputDir = "kdl/tests/test_cases/input"

expectedDir = "kdl/tests/test_cases/expected_kdl"

main :: IO ()
main = hspec $ do
  parallel $ do
    describe "should pass the kdl-org provided test cases" $ do
      files_ <- runIO $ getDirectoryContents inputDir
      let files = filter (`notElem` [".", ".."]) files_
      let inputFiles = map (inputDir </>) files
      let expectedFiles = map (expectedDir </>) files
      let testCases = zipWith testCase inputFiles expectedFiles
      sequence_ testCases

  describe "KDL.parseKDL" $ do
    it "ends a node with a semicolon or a newline" $ do
      parseKDL "node;" `shouldBe` Document
        { docNodes = [ Node { nodeName     = Identifier "node"
                            , nodeArgs     = []
                            , nodeProps    = []
                            , nodeAnn      = Nothing
                            , nodeTerminator = Semicolon
                            , nodeChildren = [] } ] }

    it "parses strings stating with r as Raw Strings" $ do
      parseKDL "node r#\"ÉPIC\\n \"RawString 😀\"#;" `shouldBe` Document
        { docNodes = [ Node { nodeName     = Identifier "node"
                            , nodeArgs     = [ Value Nothing (RawStringValue "ÉPIC\\n \"RawString 😀") ]
                            , nodeProps    = []
                            , nodeAnn      = Nothing
                            , nodeTerminator = Semicolon
                            , nodeChildren = [] } ] }


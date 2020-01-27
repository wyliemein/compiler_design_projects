{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Common

import Test.Tasty
import Paths_boa

main :: IO ()
main = do
  testsFile     <- getDataFileName "tests/adder.json"
  boaTestsFile  <- getDataFileName "tests/boa.json"
  anfTestsFile  <- getDataFileName "tests/anf.json"
  yourTestsFile <- getDataFileName "tests/yourTests.json"

  anfTests   <- readTests anfTestsFile
  adderTests <- readTests testsFile
  boaTests   <- readTests boaTestsFile
  yourTests  <- readTests yourTestsFile

  let tests = testGroup "Tests" $
                [ testGroup "Normalizer"  anfTests
                , testGroup "Adder"       adderTests
                , testGroup "Boa"         boaTests
                , testGroup "Your-Tests"  yourTests
                ]
  defaultMain tests

readTests :: FilePath -> IO [TestTree]
readTests f = map createTestTree <$> parseTestFile f

#!/usr/bin/env stack
-- stack --resolver nightly-2015-08-03 runghc --package mockery --package setenv

{-# OPTIONS_GHC -Wall -Werror #-}

import           Data.List
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
import           System.SetEnv
import           Test.Mockery.Directory

main :: IO ()
main = do
  unsetEnv "GHC_PACKAGE_PATH"
  callCommand "cabal clean"
  callCommand "cabal sdist"
  distDir <- canonicalizePath "./dist"
  tarballs <-
    map (distDir </>) <$>
    filter (".tar.gz" `isSuffixOf`) <$>
    getDirectoryContents "dist"
  case tarballs of
    [tarball] -> testInstallation tarball
    _ -> die ("tarballs: " ++ (show tarballs))

testInstallation :: FilePath -> IO ()
testInstallation tarball = inTempDirectory $ do
  callCommand ("aunpack " ++ tarball)
  callCommand "tree"
  dir <- canonicalizePath "."
  [srcDir] <-
    filter (not . ("." `isPrefixOf`)) <$>
    getDirectoryContents dir
  setCurrentDirectory srcDir
  mapM_ callCommand $
    "tinc" :
    "cabal build" :
    "cabal test" :
    ("cabal install --prefix " ++ dir) :
    []

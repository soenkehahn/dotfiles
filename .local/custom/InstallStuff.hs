#!/usr/bin/env stack
{- stack runghc
--resolver lts-13.1 --install-ghc
--package shake
-}

{-# LANGUAGE ViewPatterns #-}

import Development.Shake
import System.Directory
import System.FilePath

inHomeDir :: IO () -> IO ()
inHomeDir action = do
  homeDir <- getHomeDirectory
  setCurrentDirectory homeDir
  action

(%>>) :: FilePath -> Action () -> Rules ()
executablePath %>> rule = do
  want [executablePath]
  executablePath %> \ _ -> rule

main :: IO ()
main = inHomeDir $ shakeArgs shakeOptions $ do
  ".cargo/bin/rustup" %>> do
    cmd Shell
      "curl https://sh.rustup.rs -sSf | sh -s -- --no-modify-path -y"

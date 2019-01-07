#!/usr/bin/env stack
{- stack runghc
--resolver lts-13.1 --install-ghc
--package shake
-}

import Development.Shake
import System.Directory
import System.FilePath

inHomeDir :: IO () -> IO ()
inHomeDir action = do
  homeDir <- getHomeDirectory
  setCurrentDirectory homeDir
  action

main :: IO ()
main = inHomeDir $ shakeArgs shakeOptions $ do
    ".local/bin/parcel" %>> do
      cmd "yarn global add parcel-bundler --prefix" ".local"

    ".cargo/bin/rustup" %>> do
      cmd Shell
        "curl https://sh.rustup.rs -sSf | sh -s -- --no-modify-path -y"

(%>>) :: FilePath -> Action () -> Rules ()
executablePath %>> rule = do
  want [executablePath]
  executablePath %> \ _ -> rule

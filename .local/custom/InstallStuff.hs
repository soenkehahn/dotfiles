#!/usr/bin/env stack
{- stack runghc
--resolver lts-13.1 --install-ghc
--package shake
-}

import Development.Shake
import System.Directory
import System.FilePath

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  shakeArgs shakeOptions $ do

    (homeDir </> ".local/bin/parcel") %>> do
      cmd "yarn global add parcel-bundler --prefix"
        (homeDir </> ".local")

    (homeDir </> ".cargo/bin/rustup") %>> do
      cmd Shell
        "curl https://sh.rustup.rs -sSf | sh -s -- --no-modify-path -y"

(%>>) :: FilePath -> Action () -> Rules ()
executablePath %>> rule = do
  want [executablePath]
  executablePath %> \ _ -> rule

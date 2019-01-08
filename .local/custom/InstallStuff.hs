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
    ".local/bin/parcel" %>> do
      cmd "yarn global add parcel-bundler --prefix" ".local"

    ".cargo/bin/rustup" %>> do
      cmd Shell
        "curl https://sh.rustup.rs -sSf | sh -s -- --no-modify-path -y"

    ".cargo/bin/i3-lock-and-suspend" %>> do
      need [".shake/git-repos/i3-lock-and-suspend/Cargo.toml"]
      unit $ cmd (Cwd ".shake/git-repos/i3-lock-and-suspend")
        "cargo install --path . --force"

    ".shake/git-repos/i3-lock-and-suspend/Cargo.toml" %> \ _ -> do
      cmd (Cwd ".shake/git-repos")
        "git clone https://github.com/soenkehahn/i3-lock-and-suspend"

#!/usr/bin/env stack
-- stack --resolver lts-3.9 --install-ghc runghc

{-# LANGUAGE ViewPatterns #-}

import System.Process

main :: IO ()
main = do
  callCommand ("sudo apt-get install -y " ++ unwords packages)

packages :: [String]
packages =
  "ack-grep" :
  "chromium-browser" :
  "firefox" :
  "git" :
  "htop" :
  "i3lock" :
  "konsole" :
  "numlockx" :
  "okular" :
  "ri-li" :
  "tree" :
  "wajig" :
  "zsh" :
  "deborphan" :
  []

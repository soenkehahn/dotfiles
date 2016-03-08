#!/usr/bin/env stack
-- stack --resolver lts-3.10 --install-ghc runghc --package logging-facade --package split --package shake --package safe

{-# LANGUAGE ViewPatterns #-}

import           Control.Arrow
import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Development.Shake
import           Safe
import qualified System.Logging.Facade as Log
import           System.Process

main :: IO ()
main = do
  installedPackages <- getInstalledPackages
  case (packages \\ installedPackages) of
    [] -> return ()
    toBeInstalled -> do
      Log.info ("installing " ++ unwords toBeInstalled)
      unit $ cmd "sudo apt-get install -y" toBeInstalled

packages :: [String]
packages =
  "mhwaveedit" :
  "gparted" :
  "xloadimage" :
  "jq" :
  "handbrake" :
  "sgt-puzzles" :
  "gimp" :
  "ack-grep" :
  -- "aqualung" :
  "arandr" :
  "atool" :
  "build-essential" :
  "graphviz" :
  "chromium-browser" :
  "cmake" :
  "deborphan" :
  "dolphin" :
  "dosbox" :
  "epiphany-browser" :
  "firefox" :
  "geeqie" :
  "gist" :
  "git" :
  "git-cola" :
  "gitk" :
  "gnupg-agent" :
  "htop" :
  "i3lock" :
  "inotify-tools" :
  "iotop" :
  "jnettop" :
  "kde-baseapps-bin" :
  "konsole" :
  "libx11-dev" :
  "libxext-dev" :
  "libxft-dev" :
  "libxinerama-dev" :
  "libxrandr-dev" :
  "libxss-dev" :
  "lxde" :
  "meld" :
  "mosh" :
  "numlockx" :
  "okteta" :
  "okular" :
  "pavucontrol" :
  "powertop" :
  "pwgen" :
  "redshift" :
  "renameutils" :
  "ri-li" :
  "rtorrent" :
  "smplayer" :
  "mplayer2" :
  "tree" :
  "vlc" :
  "wajig" :
  "xmonad" :
  "xtail" :
  "zlib1g-dev" :
  "zsh" :
  []

getInstalledPackages :: IO [String]
getInstalledPackages = do
  Stdout output <- cmd "dpkg -l"
  return $ parse output
  where
    parse =
      lines >>>
      map (stripPrefix "ii  ") >>> catMaybes >>>
      map (splitOneOf " :" >>> headMay) >>> catMaybes

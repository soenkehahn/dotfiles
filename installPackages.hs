#!/usr/bin/env stack
{-
stack runghc
--resolver lts-3.10 --install-ghc
  --package logging-facade
  --package split
  --package shake
  --package safe
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE ViewPatterns #-}

import           Control.Arrow
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Development.Shake
import           Safe
import qualified System.Logging.Facade as Log

main :: IO ()
main = do
  installAptPackages
  installXMonad

installAptPackages :: IO ()
installAptPackages = do
  installedPackages <- getInstalledPackages
  case (packages \\ installedPackages) of
    [] -> return ()
    toBeInstalled -> do
      Log.info ("installing " ++ unwords toBeInstalled)
      unit $ cmd "sudo apt-get install -y" toBeInstalled

installXMonad :: IO ()
installXMonad = do
  unit $ cmd (Cwd "/home/shahn/.xmonad") "stack install xmobar"
  unit $ cmd (Cwd "/home/shahn/.xmonad") "stack build --only-dependencies"
  unit $ cmd (Cwd "/home/shahn/.xmonad") "make -f geany"

packages :: [String]
packages =
  "silversearcher-ag" :
  "docker-compose" :
  "fatsort" :
  "sox" :
  "cups" :
  "xsel" :
  "sloccount" :
  "inkscape" :
  "mercurial" :
  "mhwaveedit" :
  "libtool-bin" :
  "m4" :
  "gparted" :
  "xloadimage" :
  "libiw-dev" :
  "apt-file" :
  "libxpm-dev" :
  "jq" :
  "handbrake" :
  "sgt-puzzles" :
  "gimp" :
  "ack-grep" :
  "arandr" :
  "atool" :
  "build-essential" :
  "automake" :
  "graphviz" :
  "screen" :
  "stellarium" :
  "gdebi" :
  "chromium-browser" :
  "cmake" :
  "ubuntu-gnome-desktop" :
  "openjdk-8-jdk" :
  "deborphan" :
  "dolphin" :
  "openvpn" :
  "tig" :
  "npm" :
  "libx11-protocol-other-perl" :
  "libx11-windowhierarchy-perl" :
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
  "mplayer" :
  "tree" :
  "vlc" :
  "wajig" :
  "xmonad" :
  "whois" :
  "xtail" :
  "zlib1g-dev" :
  "vagrant" :
  "virtualbox" :
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
-- nvim commits:
--   - e38cbb93670272d0da15c60222a123b88ec55002
--   - c8d830e896e5db94ede78143866198c92645b2ba
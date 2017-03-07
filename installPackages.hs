#!/usr/bin/env stack
{-
stack runghc
--resolver lts-8.3 --install-ghc
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
  unit $ cmd (Cwd "/home/shahn/.xmonad") "stack setup"
  unit $ cmd (Cwd "/home/shahn/.xmonad") "stack install xmobar"
  unit $ cmd (Cwd "/home/shahn/.xmonad") "stack build --only-dependencies"
  unit $ cmd (Cwd "/home/shahn/.xmonad") "make -f geany"

packages :: [String]
packages =
  "ack-grep" :
  "apt-file" :
  "arandr" :
  "atool" :
  "automake" :
  "build-essential" :
  "chromium-browser" :
  "cmake" :
  "cups" :
  "deborphan" :
  "docker-compose" :
  "dolphin" :
  "dos2unix" :
  "dosbox" :
  "epiphany-browser" :
  "fatsort" :
  "firefox" :
  "gdebi" :
  "geeqie" :
  "gimp" :
  "gist" :
  "git" :
  "git-cola" :
  "gitk" :
  "gnupg-agent" :
  "gparted" :
  "graphviz" :
  "handbrake" :
  "htop" :
  "i3lock" :
  "inkscape" :
  "inotify-tools" :
  "iotop" :
  "jnettop" :
  "jq" :
  "kcolorchooser" :
  "kde-baseapps-bin" :
  "kig" :
  "konsole" :
  "kruler" :
  "ksnapshot" :
  "libiw-dev" :
  "libtool-bin" :
  "libx11-dev" :
  "libx11-protocol-other-perl" :
  "libx11-windowhierarchy-perl" :
  "libxext-dev" :
  "libxft-dev" :
  "libxinerama-dev" :
  "libxpm-dev" :
  "libxrandr-dev" :
  "libxss-dev" :
  "lxde" :
  "m4" :
  "meld" :
  "mercurial" :
  "mhwaveedit" :
  "mosh" :
  "mplayer" :
  "npm" :
  "numlockx" :
  "okteta" :
  "okular" :
  "openjdk-8-jdk" :
  "openvpn" :
  "pavucontrol" :
  "pdftk" :
  "postgresql-client-9.5" :
  "postgresql-client-common" :
  "powertop" :
  "pwgen" :
  "redshift" :
  "renameutils" :
  "ri-li" :
  "rtorrent" :
  "screen" :
  "sgt-puzzles" :
  "silversearcher-ag" :
  "sl" :
  "sloccount" :
  "smplayer" :
  "sox" :
  "stellarium" :
  "tig" :
  "tree" :
  "ubuntu-gnome-desktop" :
  "vagrant" :
  "vim" :
  "virtualbox" :
  "vlc" :
  "wajig" :
  "whois" :
  "xloadimage" :
  "xmonad" :
  "xsel" :
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
-- nvim commits:
--   - e38cbb93670272d0da15c60222a123b88ec55002
--   - c8d830e896e5db94ede78143866198c92645b2ba

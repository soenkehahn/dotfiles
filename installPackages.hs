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
import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Development.Shake hiding (doesFileExist)
import           Safe
import           System.Directory
import           System.FilePath
import qualified System.Logging.Facade as Log

main :: IO ()
main = do
  upgradeStack
  installAptPackages
  installXMonad
  installCustom
  installSlack

upgradeStack :: IO ()
upgradeStack = do
  unit $ cmd "stack upgrade"
  unit $ cmd "stack upgrade --binary-version 1.5.1"

installSlack :: IO ()
installSlack = do
  let debFile = "slack-desktop-2.8.1-amd64.deb"
  exists <- doesFileExist ("/home/shahn/.local" </> debFile)
  when (not exists) $ do
    unit $ cmd [ Cwd "/home/shahn/.local" ] "wget" "https://downloads.slack-edge.com/linux_releases/slack-desktop-2.8.1-amd64.deb"
  unit $ cmd [ Cwd "/home/shahn/.local" ] "ls"
  unit $ cmd [ Cwd "/home/shahn/.local" ] "sudo gdebi --non-interactive" debFile

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
  "apt-file" :
  "docker" :
  "pv" :
  "krita" :
  "arandr" :
  "nethogs" :
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
  "mkvtoolnix" :
  "kruler" :
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
  "openssh-server" :
  "openvpn" :
  "pavucontrol" :
  "pdftk" :
  "postgresql-client-9.5" :
  "postgresql-client-common" :
  "powertop" :
  "pwgen" :
  "redshift" :
  "pm-utils" :
  "krusader" :
  "renameutils" :
  "ri-li" :
  "rtorrent" :
  "screen" :
  "sgt-puzzles" :
  "silversearcher-ag" :
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

installCustom :: IO ()
installCustom = do
  unit $ cmd (Cwd "/home/shahn/.local/custom") "./install.sh"

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}

module XMobar (mkConfig) where

import           Data.List
import           XMonad
import           XMonad.Hooks.DynamicLog

mkConfig customConfig =
  statusBar "xmobar" XMobar.config toggleStrutsKey customConfig

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)

config :: PP
config = xmobarPP {
  ppCurrent = xmobarColor "#dd2e2e" "" . wrap "<" ">",
  ppVisible = \ ws -> xmobarColor "#93e0e3" "" (" " ++ ws ++ " "),
  ppHidden = \ ws -> " " ++ ws ++ " ",
  ppWsSep = "",
  ppUrgent = formatUrgentWindows,
  ppOrder = \ (windows : _) -> [formatWindows windows]
}

formatUrgentWindows :: String -> String
formatUrgentWindows = \ case
  "NSP" -> xmobarColor "magenta" "" $ xmobarStrip "[bell] "
  string -> xmobarColor "magenta" "" $ xmobarStrip string

formatWindows :: String -> String
formatWindows string =
  (if " NSP " `isPrefixOf` string then drop 5 else id)
  string

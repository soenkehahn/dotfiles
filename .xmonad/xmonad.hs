{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Main where


import           Prelude                     hiding (catch, mapM_)

import           Data.Foldable               (mapM_)
import           Data.List
import           Data.Map                    (fromList)
import           Data.Monoid
import           Data.Ratio
import           Data.Set                    (member)

import           Text.Printf

import           Control.Applicative         ((<$>))
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad               (filterM, when)

import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
import           System.Random
import           System.Time

import           XMonad                      hiding (focus)
import           XMonad.Layout.Tabbed        (shrinkText, tabbedAlways)
import           XMonad.Prompt               (XPConfig (..),
                                              XPPosition (Bottom),
                                              defaultXPConfig)
import           XMonad.Prompt.RunOrRaise    (runOrRaisePrompt)
import           XMonad.StackSet             (RationalRect (..), Stack (..),
                                              current, focusDown, greedyView,
                                              screen, screens, sink, stack,
                                              swapDown, swapUp, view, workspace)
import           XMonad.Util.Themes          (smallClean, theme)

import           XMonad.Hooks.InsertPosition (Focus (Newer, Older),
                                              Position (Above, Below),
                                              insertPosition)
import           XMonad.Hooks.ManageDocks    (avoidStruts)

import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.Scratchpad

import           System.IO
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.UrgencyHook


main = do
    spawn "redshift -l 1.17:103.5 -r"
    spawn "/home/shahn/neo/asdf"
    let bgi = "~/background.png"
    spawn ("xloadimage -onroot -fullscreen " ++ bgi)
    xmobar <- spawnPipe "~/.nix-profile/bin/xmobar"
    xmonad $
        withUrgencyHook NoUrgencyHook $
        myConfig {
            logHook = dynamicLogWithPP $ xmobarPP {
                ppCurrent = xmobarColor "#dd2e2e" "" . wrap "<" ">",
                ppVisible = \ ws -> xmobarColor "#93e0e3" "" (" " ++ ws ++ " "),
                ppHidden = \ ws -> " " ++ ws ++ " ",
                ppWsSep = "",
                ppUrgent = xmobarColor "magenta" "black" . xmobarStrip,
                ppOutput = \ string ->
                    hPutStrLn xmobar $
                    (if " NSP " `isPrefixOf` string then drop 5 else id) string,
                ppTitle = const "",
                ppLayout = const ""
                -- ppSort = return reverse
              }
          }

myConfig = defaultConfig {
      -- simple stuff
        terminal           = "konsole",
        borderWidth        = 2,
        focusedBorderColor = "#444444",
        normalBorderColor  = "#000000",
        modMask            = mod4Mask,
        XMonad.workspaces  = "NSP" : fmap show [1 .. 9],
        keys               = myKeys,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myHandleEventHook
    }


myLayout =
        avoidStruts defaultTabbed
    ||| Full
    ||| avoidStruts (TwoPane 0.03 0.5)
defaultTabbed = tabbedAlways shrinkText (theme smallClean)


myKeys conf =
    let modKey = modMask conf
    in fromList $
    -- start programs
    ((modKey, xK_n), runOrRaisePrompt runOrRaiseConfig) :
    ((modKey, xK_r), spawn $ terminal conf) :
    ((modKey, xK_t), spawn "firefox") :
    ((modKey, xK_d), spawn "dolphin") :

    -- switch between programs
    ((modKey, xK_i), withWindowStack focusToLeft) :
    ((modKey, xK_Left), withWindowStack focusToLeft) :
    ((modKey .|. controlMask, xK_i), windows swapUp) :
    ((modKey .|. controlMask, xK_Left), windows swapUp) :
    ((modKey, xK_e), withWindowStack focusToRight) :
    ((modKey, xK_Right), withWindowStack focusToRight) :
    ((modKey .|. controlMask, xK_e), windows swapDown) :
    ((modKey .|. controlMask, xK_Right), windows swapDown) :

    -- close windows
    ((modKey, xK_x), kill) :

    -- switch to fullscreen
    ((modKey, xK_a), sendMessage NextLayout) :

    -- resize windows
    ((modKey, xK_c), sendMessage Expand) :
    ((modKey, xK_l), sendMessage Shrink) :


    -- --------------
    -- shutdown, etc.
    -- --------------

    -- exit and restart xmonad
    ((modKey .|. controlMask, xK_x), io (exitWith ExitSuccess)) :
    ((modKey .|. controlMask, xK_r), restart "xmonad" True) :

    -- suspend
    ((modKey .|. controlMask, xK_u), spawn "i3lock & sudo pm-suspend") :

    -- shutdown
    ((modKey .|. controlMask, xK_h), spawn "sudo halt") :

    -- lock screen
    ((modKey .|. controlMask, xK_l), spawn "i3lock") :

    ((modKey, xK_v), withFocused $ windows . sink) :

    -- debugging
--     ((modKey .|. controlMask, xK_d), logWindowStack) :


    -- ----------- --
    -- scratchpads --
    -- ----------- --

    ((modKey, xK_s), namedScratchpadAction scratchpads "vim") :
    ((modKey, xK_h), namedScratchpadAction scratchpads "htop") :
    ((modKey, xK_g), namedScratchpadAction scratchpads "aqualung") :
    ((modKey, xK_f), namedScratchpadAction scratchpads "musicSelection") :
    ((modKey, xK_q), namedScratchpadAction scratchpads "pavucontrol") :

    -- -------------------
    -- Workspace switching
    -- -------------------
    (fmap (\ (workspace, key) ->
        ((modKey, key), windows (greedyView workspace)))
        (zip (workspaces conf) [xK_0 .. xK_9])) ++

    -- screen switching
    ((modKey, xK_m), do
        currentScreen <- screen <$> current <$> windowset <$> get
        availableScreens <- fmap screen <$> screens <$> windowset <$> get
        when (currentScreen `elem` availableScreens) $
            case dropWhile (/= currentScreen) (cycle availableScreens) of
                (_ : next : r) -> do
                    ws <- screenWorkspace next
                    whenJust ws (windows . view)
                _ -> return ()) :

    []

myManageHook =
    insertPosition Above Newer
    <+> namedScratchpadManageHook scratchpads
    <+> specialManageHooks

specialManageHooks = composeAll $
    -- (className =? "Gimp" --> doFloat) :
    []

scratchpads :: [NamedScratchpad]
scratchpads =
    NS "vim" (terminal myConfig ++ " --name vim") (appName =? "vim") centerBig :
    NS "htop" (terminal myConfig ++ " --name htopTerminal") (appName =? "htopTerminal") centerBig :
    NS "aqualung" "aqualung" (appName =? "aqualung") centerBig :
    NS "musicSelection"
        (terminal myConfig ++ " --name musicSelection --workdir /home/shahn/musik/beets")
        (appName =? "musicSelection") centerBig :
    NS "pavucontrol" "pavucontrol" (appName =? "pavucontrol") centerBig :
    []
  where
    screenRatio = width % height
    height = 800
    width = 1280

    centerBig :: ManageHook
    centerBig = customFloating $ RationalRect
        (padding width) (padding height) (nonPadding width) (nonPadding height)
      where
        padding s = 50 % s
        nonPadding s = 1 - 2 * padding s


myHandleEventHook :: Event -> X All
myHandleEventHook e@DestroyWindowEvent{} = do
    s <- get
    let isAtLeft = case stack $ workspace $ current $ windowset s of
            Nothing -> False
            Just stack_ -> null $ down stack_
    return $ All True
myHandleEventHook _ = return $ All True

logWindowStack :: X ()
logWindowStack = do
    s <- get
    xlog $ show $ stack $ workspace $ current $ windowset s

-- replace with XMonad.Actions.CycleWindows?

focusToLeft s@(Stack focus [] _) = s
focusToLeft (Stack focus (a : r) down) = Stack a r (focus : down)

withFocusToLeft s@(Stack focus [] _) = s
withFocusToLeft (Stack focus (a : r) down) = Stack focus r (a : down)

focusToRight s@(Stack focus _ []) = s
focusToRight (Stack focus up (a : r)) = Stack a (focus : up) r

withFocusToRight s@(Stack focus _ []) = s
withFocusToRight (Stack focus up (a : r)) = Stack focus (a : up) r


runOrRaiseConfig :: XPConfig
runOrRaiseConfig = defaultXPConfig {
    font              = "xft:Ubuntu Mono:pixelsize=20,style=regular",
    bgColor           = "#2c2c2c",
    fgColor           = "#dcdccc",
    fgHLight          = "#dd2e2e",
    bgHLight          = "#2c2c2c",
    borderColor       = "#444444",
    promptBorderWidth = 2,
    position          = Bottom,
    height            = 26,
    historySize       = 256,
    historyFilter     = nub,
    defaultText       = ""
  }


-- * Utils

xlog msg =
    io $ appendFile "/tmp/xmonadLog" (msg ++ "\n")

xprint :: Show s => s -> X ()
xprint = xlog . show


withWindowStack :: (Stack Window -> Stack Window) -> X ()
withWindowStack fun =
    windows $ \ ws ->
        let mstack = getStack ws
        in case mstack of
            Nothing -> ws
            Just stack -> setStack ws (fun stack)

getStack :: WindowSet -> Maybe (Stack Window)
getStack = stack . workspace . current

setStack :: WindowSet -> Stack Window -> WindowSet
setStack set stack' =
    set{current = current'}
  where current' = oldCurrent{workspace = workspace'}
        oldCurrent = current set
        workspace' = oldWorkspace{stack = Just stack'}
        oldWorkspace = workspace oldCurrent


swapProcess :: String -> X ()
swapProcess cmd = do
    dir <- getXMonadDir
    let lockFile = dir </> cmd <.> "lock"
    exists <- io $ doesFileExist lockFile
    xprint exists
    if exists then do
        spawn ("killall " ++ cmd)
        io $ removeFile lockFile
      else do
        io $ writeFile lockFile "lock!"
        spawn cmd

xShowMessage :: String -> X ()
xShowMessage msg =
    spawn ("kdialog --msgbox \"" ++ msg ++ "\"")


-- * My own version of TwoPane (not used?)

data TwoPane a =
    TwoPane Rational Rational
    deriving ( Show, Read )

instance LayoutClass TwoPane a where
    doLayout (TwoPane _ split) r s = return (arrange r s,Nothing)
        where
          arrange rect st = case reverse (up st) of
                              (master:_) -> [(master,left),(focus st,right)]
                              [] -> case down st of
                                      (next:_) -> [(focus st,left),(next,right)]
                                      [] -> [(focus st, rect)]
              where (left, right) = splitHorizontallyBy split rect

    handleMessage (TwoPane delta split) x =
        return $ case fromMessage x of
                   Just Shrink -> Just (TwoPane delta (split - delta))
                   Just Expand -> Just (TwoPane delta (split + delta))
                   _           -> Nothing

    description _ = "TwoPane"


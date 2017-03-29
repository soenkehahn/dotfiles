{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}

module Main (main) where

import           Prelude hiding (mapM_)

import           Control.Monad
import           Data.List
import           Data.Map (Map, fromList)
import           Data.Ratio
import           System.Directory
import           System.Environment
import           System.Exit
import           XMonad
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Decoration
import           XMonad.Layout.DragPane
import           XMonad.Layout.Grid
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPane
import           XMonad.Prompt
import           XMonad.Prompt.RunOrRaise
import           XMonad.StackSet hiding (workspaces)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Themes (smallClean, theme)

import           XMobar
-- import           TabTree

initialize :: IO ()
initialize = do
  path <- getEnv "PATH"
  setEnv "PATH" ("/home/shahn/.local/bin:" ++ path)

  spawn "redshift -l 40.7:-74.0 -r" -- New York

  let bgi = "/home/shahn/.xmonad/background.png"
  exists <- doesFileExist bgi
  when exists $ do
    spawn ("xloadimage -onroot -fullscreen " ++ bgi)

  spawn "konsole -e ~/.xmonad/start.sh"

main :: IO ()
main = do
    Main.initialize
    xmonad =<< XMobar.mkConfig myConfig

myConfig = withUrgencyHook NoUrgencyHook $ def {
      -- simple stuff
        terminal = "konsole",
        borderWidth = 3,
        focusedBorderColor = "#8A999E",
        normalBorderColor = "#363B4B",
        modMask = mod4Mask,
        XMonad.workspaces  = "NSP" : fmap show [1 .. 9 :: Int],
        keys = myKeys,

      -- hooks, layouts
        startupHook = setWMName "LG3D",
        layoutHook = avoidStruts myLayout,
        manageHook = myManageHook
    }

myLayout =
      defaultTabbed
  ||| Full
  ||| Grid
  ||| TwoPane (3/100) (1/2)
--  ||| avoidStruts (TwoPane 0.03 0.5)
--  ||| ML
--  ||| TabTree
--  ||| tabTree shrinkText defaultTheme
--  ||| (UninitializedTabTree :: TabTree a)

defaultTabbed = tabbedAlways shrinkText (theme smallClean)

-- * shortcuts

type KeyMap = Map (KeyMask, KeySym) (X ())

myKeys :: XConfig l -> KeyMap
myKeys conf =
    let modKey = modMask conf
    in fromList $

    -- start programs
    ((modKey, xK_n), runOrRaisePrompt runOrRaiseConfig) :
    ((modKey, xK_r), spawn $ terminal conf) :
    ((modKey, xK_t), spawn "chromium-browser") :
    ((modKey, xK_d), spawn "dolphin") :

    -- ----------- --
    -- scratchpads --
    -- ----------- --

    ((modKey, xK_s), namedScratchpadAction scratchpads "vim") :
    ((modKey, xK_h), namedScratchpadAction scratchpads "htop") :
    ((modKey, xK_g), namedScratchpadAction scratchpads "g-scratchpad") :
    ((modKey, xK_f), namedScratchpadAction scratchpads "musicSelection") :
    ((modKey, xK_q), namedScratchpadAction scratchpads "pavucontrol") :

    -- switch between programs
    ((modKey, xK_i), withWindowStack focusToLeft) :
    ((modKey, xK_Left), withWindowStack focusToLeft) :
    ((modKey .|. controlMask, xK_i), windows swapUp) :
    ((modKey .|. controlMask, xK_Left), windows swapUp) :
    ((modKey, xK_e), withWindowStack focusToRight) :
    ((modKey, xK_Right), withWindowStack focusToRight) :
    ((modKey .|. controlMask, xK_e), windows swapDown) :
    ((modKey .|. controlMask, xK_Right), windows swapDown) :

    workspaceSwitching conf modKey ++

    -- screen switching
    ((modKey, xK_m), do
        currentScreen <- screen <$> current <$> windowset <$> get
        availableScreens <- fmap screen <$> screens <$> windowset <$> get
        when (currentScreen `elem` availableScreens) $
            case dropWhile (/= currentScreen) (cycle availableScreens) of
                (_ : next : _) -> do
                    ws <- screenWorkspace next
                    whenJust ws (windows . view)
                _ -> return ()) :

    -- close windows
    ((modKey, xK_x), kill) :

    -- switch to fullscreen
    ((modKey, xK_a), sendMessage NextLayout) :

    -- resize windows
    ((modKey, xK_c), sendMessage Expand) :
    ((modKey, xK_l), sendMessage Shrink) :

    -- change number of windows in master pane
    ((modKey, xK_comma), sendMessage (IncMasterN 1)) :
    ((modKey, xK_period), sendMessage (IncMasterN (- 1))) :

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

    []

workspaceSwitching :: XConfig l -> KeyMask
  -> [((KeyMask, KeySym), X ())]
workspaceSwitching conf modKey =
    concat $
    (flip fmap)
      (zip (workspaces conf) [xK_0 .. xK_9]) $ \ (workspace, key) ->
        ((modKey, key), windows (greedyView workspace)) :
        ((controlMask .|. modKey, key), windows (greedyView workspace . shift workspace)) :
        []

myManageHook :: ManageHook
myManageHook =
  namedScratchpadManageHook scratchpads <+>
  specialManageHooks <+>
  insertPosition Above Newer

specialManageHooks :: ManageHook
specialManageHooks = composeAll $
    -- (className =? "Gimp" --> doFloat) :
    (ask >>= doF . sink) :
    []

scratchpads :: [NamedScratchpad]
scratchpads =
    NS "vim" (terminal myConfig ++ " --name vim") (appName =? "vim") centerBig :
    NS "htop" (terminal myConfig ++ " --name htopTerminal") (appName =? "htopTerminal") centerBig :
    NS "g-scratchpad" "kdialog --sorry NYI" (appName =? "foo") nonFloating :
    NS "musicSelection"
        (terminal myConfig ++ " --name musicSelection --workdir ~/musik/beets")
        (appName =? "musicSelection") centerBig :
    NS "pavucontrol" "pavucontrol" (appName =? "pavucontrol") centerBig :
    []
  where
    height = 800
    width = 1280

    centerBig :: ManageHook
    centerBig = customFloating $ RationalRect
        (padding width) (padding height) (nonPadding width) (nonPadding height)
      where
        padding s = 50 % s
        nonPadding s = 1 - 2 * padding s

-- replace with XMonad.Actions.CycleWindows?

focusToLeft :: Stack a -> Stack a
focusToLeft s@(Stack _focus [] _) = s
focusToLeft (Stack focus (a : r) down) = Stack a r (focus : down)

focusToRight :: Stack a -> Stack a
focusToRight s@(Stack _focus _ []) = s
focusToRight (Stack focus up (a : r)) = Stack a (focus : up) r

runOrRaiseConfig :: XPConfig
runOrRaiseConfig = def {
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


-- * ML

data ML a = ML
  deriving (Show, Read)

instance LayoutClass ML a where
  description _ = "ML"
  pureMessage ML _ = Nothing
  pureLayout _ screenRectangle@(Rectangle _x _y _width _height) stack =
    zip (integrate stack) (mkGrid screenRectangle (length (integrate stack)))

mkGrid :: Rectangle -> Int -> [Rectangle]
mkGrid screen n =
  concat $
  for [0..pred gridHeight] $ \ gridY ->
    for [0..pred gridWidth] $ \ gridX ->
      Rectangle (fromIntegral (gridX * cellWidth)) (fromIntegral (gridY * cellHeight))
                (fromIntegral cellWidth) (fromIntegral cellHeight)
 where
  squareRoot :: Double
  squareRoot = sqrt (realToFrac n)
  gridHeight, gridWidth :: Int
  gridHeight = ceiling (n // gridWidth)
  gridWidth = ceiling squareRoot

  cellWidth, cellHeight :: Int
  cellWidth = fromIntegral (rect_width screen) `div` gridWidth
  cellHeight = fromIntegral (rect_height screen) `div` gridHeight

for :: [a] -> (a -> b) -> [b]
for = flip map

(//) :: Int -> Int -> Double
a // b = realToFrac a / realToFrac b

module Lib
    ( someFunc
    ) where
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Util.EZConfig
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Util.NamedWindows (getName)
import Data.List
import System.IO
import Data.Function (on)
import Control.Monad (join)
import XMonad.Layout.NoFrillsDecoration
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CopyWindow
import XMonad.Layout.PerScreen              -- Check screen width & adjust layouts
import XMonad.Layout.PerWorkspace           -- Configure layouts on a per-workspace
import XMonad.Util.NamedScratchpad

myLogHook h = do

    -- following block for copy windows marking
    copies <- wsContainingCopies
    let check ws | ws `elem` copies =
                   pad . xmobarColor green red . wrap "*" " "  $ ws
                 | otherwise = pad ws

    ewmhDesktopsLogHook
    --dynamicLogWithPP $ defaultPP
    dynamicLogWithPP $ def
        { ppCurrent             = wrap "[" "]"
        , ppTitle               = xmobarColor green "" 
        , ppVisible             = wrap "(" ")"
        , ppUrgent              = xmobarColor red    "" . wrap " " " "
        , ppHidden              = check
        , ppHiddenNoWindows     = const ""
        , ppSep                 = xmobarColor "white" backgroundColor "  :  "
        , ppWsSep               = " "
        , ppLayout              = xmobarColor green "" . dropWhile (/=' ')
        , ppOrder               = id
        , ppOutput              = hPutStrLn h  
        , ppSort                = fmap 
                                  (namedScratchpadFilterOutWorkspace.)
                                  (ppSort def)
                                  --(ppSort defaultPP)
        , ppExtras              = [] }

myFont = "xft:Noto Sans CJK:size=10:antialias=true"
backgroundColor = "#1d262b"
red = "#ba2922"
red1 = "#cc372c"
blue = "#16a085"
blue1 = "#13bf9d"
green = "#43746a"
green1 = "#487d72"

myTheme = def 
  { inactiveBorderColor   = backgroundColor
  , inactiveColor         = backgroundColor
  , inactiveTextColor     = backgroundColor
  , activeBorderColor     = blue
  , activeColor           = blue
  , activeTextColor       = blue
  , urgentBorderColor     = red
  , urgentTextColor       = green
  , decoHeight            = 4
  }

myLayoutHook = avoidStruts $ noFrillsDeco shrinkText myTheme $ Tall 1 0.03 0.6  ||| Full
-- myLayoutHook = avoidStruts $ noBorders ( Tall 1 0.03 0.6  ||| Full)

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myStartupHook = spawn "compton -b" >> spawn "unclutter -b"

myEventLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss
  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")
  where fmt currWs ws
          | currWs == ws = "[" ++ ws ++ "]"
          | otherwise    = " " ++ ws ++ " "
        sort' = sortBy (compare `on` (!! 0))


myConfig p = def 
        { manageHook = manageDocks <+> manageHook def
        , modMask = mod4Mask
        , layoutHook = myLayoutHook
        , terminal = "xfce4-terminal"
        , startupHook = myStartupHook
        , logHook = myLogHook p
        , borderWidth = 0
        } `additionalKeysP` myKeys

myKeys = [ ("M-p", spawn "rofi -show combi -combi-modi mofi,drun -modi 'mofi:mofi' -show-icons -matching fuzzy -sorting-method fzf")
       , ("M-f", spawn "firefox")
       ] ++ [ (otherModMasks ++ "M-" ++ [key], action tag) |
         (tag, key)  <- zip myWorkspaces "123456789" , 
         (otherModMasks, action) <- [ ("", windows . W.view), ("S-", windows . W.shift)]]

someFunc :: IO ()
someFunc = do
  xmproc <- spawnPipe "/home/aemaeth/.local/bin/xmobar /home/aemaeth/.xmonad/xmobar.conf -x 1"
  xmonad $ docks $ myConfig xmproc

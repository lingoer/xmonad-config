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

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]
-- logFile = mapM (spawn . ("mkfifo /tmp/" ++)) [".xmonad-workspace-log", ".xmonad-title-log"]
-- >> logFile >> spawn "polybar main"

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



myConfig = def 
        { manageHook = manageDocks <+> manageHook def
        , modMask = mod4Mask
        , layoutHook = avoidStruts $ noBorders ( Tall 1 0.03 0.6  ||| Full)
        , terminal = "xfce4-terminal"
        , startupHook = myStartupHook
--        , logHook = myEventLogHook
        , borderWidth = 0
        } `additionalKeysP` myKeys

myKeys = [ ("M-p", spawn "rofi -show combi -combi-modi mofi,drun -modi 'mofi:mofi' -show-icons -matching fuzzy -sorting-method fzf")
       , ("M-f", spawn "firefox")
       ] ++ [ (otherModMasks ++ "M-" ++ [key], action tag) |
         (tag, key)  <- zip myWorkspaces "123456789" , 
         (otherModMasks, action) <- [ ("", windows . W.view), ("S-", windows . W.shift)]]

someFunc :: IO ()
someFunc = xmonad $ docks myConfig

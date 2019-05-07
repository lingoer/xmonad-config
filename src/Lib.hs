module Lib
  ( someFunc
  )
where
import           XMonad                  hiding ( (|||) )
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.Run                ( spawnPipe
                                                , safeSpawn
                                                )
import           XMonad.Util.EZConfig
import           XMonad.Actions.SpawnOn
import           XMonad.Layout.NoBorders
import           XMonad.Layout.LayoutCombinators
import qualified XMonad.StackSet               as W
import           XMonad.Hooks.DynamicLog
import           XMonad.Util.NamedWindows       ( getName )
import           Data.List
import           System.IO
import           Data.Function                  ( on )
import           Control.Monad                  ( join )
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Actions.CopyWindow
import           XMonad.Layout.PerScreen              -- Check screen width & adjust layouts
import           XMonad.Layout.PerWorkspace           -- Configure layouts on a per-workspace
import           XMonad.Util.NamedScratchpad
import           XMonad.Actions.DynamicProjects

myFont = "xft:Noto Sans CJK:size=10:antialias=true"
backgroundColor = "#1d262b"
red = "#ba2922"
red1 = "#cc372c"
blue = "#16a085"
blue1 = "#13bf9d"
green = "#43746a"
green1 = "#487d72"

myTheme = def { inactiveBorderColor = backgroundColor
              , inactiveColor       = backgroundColor
              , inactiveTextColor   = backgroundColor
              , activeBorderColor   = blue
              , activeColor         = blue
              , activeTextColor     = blue
              , decoHeight          = 4
              }

myLayoutHook =
  avoidStruts (noFrillsDeco shrinkText myTheme $ Tall 1 0.03 0.6) ||| Full

projects :: [Project]
projects =
  [ Project { projectName      = "gen"
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn "xfce4-terminal"
            }
  , Project { projectName      = "web"
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn "firefox"
            }
  , Project { projectName      = "proj"
            , projectDirectory = "~/proj"
            , projectStartHook = Just $ spawn "xfce4-terminal"
            }
  , Project { projectName      = "work"
            , projectDirectory = "~/tappollo"
            , projectStartHook = Just $ spawn "xfce4-terminal"
            }
  ]
myWorkspaces = ["gen", "web", "proj", "work"]
scratchpads = 
  [ NS "slack" "slack --proxy-server=\"socks5://192.168.1.107:1081\"" (className =? "Slack") defaultFloating
  , NS "NCM" "netease-cloud-music" (className =? "netease-cloud-music") defaultFloating
  , NS "Email" "thunderbird" (className =? "Thunderbird") nonFloating
  ]
myKeys =
  [ ( "M-p"
    , spawn
      "rofi -show combi -combi-modi mofi,drun -modi 'mofi:mofi' -show-icons -matching fuzzy -sorting-method fzf"
    )
  , ( "M-S-q"
    , spawn
      "xfce4-session-logout"
    )
  , ("M-f", spawn "firefox")
  , ("M-i", namedScratchpadAction scratchpads "slack")
  , ("M-u", namedScratchpadAction scratchpads "NCM")
  , ("M-S-m", namedScratchpadAction scratchpads "Email")
  ]

myStartupHook:: X ()
myStartupHook = do
  spawn "unclutter -b"
  spawn "compton -b"
myManageHook = composeAll
  [ className =? "Wrapper-2.0" --> doFloat
  , className =? "Xfce4-notes" --> doFloat
  , namedScratchpadManageHook scratchpads
  ]

myConfig =
  def { manageHook  = myManageHook <+> manageSpawn <+> manageDocks <+> manageHook def
      , modMask     = mod4Mask
      , layoutHook  = myLayoutHook
      , terminal    = "xfce4-terminal"
      , startupHook = ewmhDesktopsStartup >> myStartupHook
      , logHook     = ewmhDesktopsLogHookCustom namedScratchpadFilterOutWorkspace -- myLogHook p
      , handleEventHook = ewmhDesktopsEventHook
      , borderWidth = 0
      , workspaces  = myWorkspaces
      }
    `additionalKeysP` myKeys


someFunc :: IO ()
someFunc = xmonad $ dynamicProjects projects $ docks myConfig

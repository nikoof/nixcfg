{- HLINT ignore "Redundant $" -}

import Control.Monad
import Data.Map qualified as M
import Data.Monoid
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Exit
import XMonad
import XMonad.Actions.NoBorders
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Rescreen
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumnStable
import XMonad.Layout.ThreeColumns
import XMonad.Operations
import XMonad.Prelude qualified as P
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.SpawnOnce
import XMonad.Util.Types

base16Colors :: [String]
base16Colors =
  [ "#@base00@",
    "#@base01@",
    "#@base02@",
    "#@base03@",
    "#@base04@",
    "#@base05@",
    "#@base06@",
    "#@base07@",
    "#@base08@",
    "#@base09@",
    "#@base0A@",
    "#@base0B@",
    "#@base0C@",
    "#@base0D@",
    "#@base0E@",
    "#@base0F@"
  ]

-- Main config + hooks
main :: IO ()
main =
  xmonad
    . addAfterRescreenHook myAfterRescreenHook
    . addRandrChangeHook myRandrChangeHook
    . ewmhFullscreen
    . ewmh
    . dynamicSBs myStatusBarSpawner
    . docks
    $ myConfig

wallpaperCmd = "feh --no-fehbg --bg-center @wallpaperPath@ @wallpaperPath@"

myConfig = myConfig' `additionalKeysP` myKeys myConfig' `removeKeysP` ["M-<Space>", "M-p"]
  where
    myConfig' =
      def
        { workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"],
          terminal = "@terminal@",
          modMask = mod4Mask,
          layoutHook =
            avoidStruts
              . renamed [KeepWordsRight 1]
              . spacingRaw False (Border 2 0 0 0) True (Border 0 0 0 0) False
              $ myLayoutHook,
          startupHook = myStartupHook,
          manageHook = myManageHook <+> namedScratchpadManageHook myScratchpads,
          normalBorderColor = base16Colors !! 0x2,
          focusedBorderColor = base16Colors !! 0xE
        }

myScratchpads =
  [ NS
      "keepassxc"
      "keepassxc"
      (className =? "KeePassXC")
      (customFloating $ W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)),
    NS
      "carla"
      "carla"
      (className =? "Carla2")
      (customFloating $ W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)),
    NS
      "pwvucontrol"
      "pwvucontrol"
      (className =? "pwvucontrol")
      (customFloating $ W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4))
  ]

myLayoutHook = masterStack ||| threeCol ||| full ||| bsp
  where
    masterStack = smartBorders . renamed [Replace "MasterStack"] $ Tall nmaster delta ratio
    threeCol = renamed [Replace "ThreeCol"] $ emptyTCS delta ratio
    full = noBorders . renamed [Replace "i3"] $ tabbed shrinkText tabTheme
    bsp = smartBorders . renamed [Replace "BSP"] $ emptyBSP

    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100
    tabTheme =
      Theme
        { activeColor = base16Colors !! 0x01,
          inactiveColor = base16Colors !! 0x00,
          urgentColor = base16Colors !! 0x08,
          activeBorderColor = base16Colors !! 0x0e,
          inactiveBorderColor = base16Colors !! 0x02,
          urgentBorderColor = base16Colors !! 0x02,
          activeBorderWidth = 1,
          inactiveBorderWidth = 1,
          urgentBorderWidth = 1,
          activeTextColor = base16Colors !! 0x07,
          inactiveTextColor = base16Colors !! 0x03,
          urgentTextColor = base16Colors !! 0x02,
          fontName = "xft:FiraCode Nerd Font Mono:size=11:antialias=true",
          decoHeight = 24,
          decoWidth = 0,
          windowTitleAddons = [],
          windowTitleIcons = []
        }

myStartupHook :: X ()
myStartupHook = do
  checkKeymap <*> myKeys $ myConfig
  spawnOnce "picom"
  spawnOnce wallpaperCmd

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "firefox" --> doShift "2",
      className =? "chromium-browser" --> doShift "2",
      className =? "discord" --> doShift "3",
      className =? "spotify" --> doShift "5",
      className =? "Gimp" --> doShift "7" <+> doFloat,
      isDialog --> doFloat,
      namedScratchpadManageHook myScratchpads
    ]

myAfterRescreenHook :: X ()
myAfterRescreenHook = spawn wallpaperCmd

myRandrChangeHook :: X ()
myRandrChangeHook = spawn "autorandr --change"

myKeys :: XConfig l -> [(String, X ())]
myKeys conf =
  let term = XMonad.terminal conf
      scrot = "sleep 0.2; scrot -zfs -F '/tmp/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f && rm $f'"
      scrotFull = "sleep 0.2; scrot -zm -F '/tmp/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f && rm $f'"
      scrotWindow = "sleep 0.2; scrot -zu -F '/tmp/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f && rm $f'"
      scrotZbar = "sleep 0.2; scrot -zfs -F '/tmp/%F_%T_$wx$h.png' -e 'zbarimg -q -1 $f | cut -f2- -d: | xclip -rmlastnl -selection clipboard -target text/plain -in && rm $f'"
      xcolor = "xcolor | xclip -rmlastnl -selection clipboard -in"
      -- TODO: Use base16Colors here
      dmenuRun = "dmenu_run -fn 'FiraCode Nerd Font Mono-12' -nb '#000000' -nf '#ffffff' -sb '#b294bb' -sf '#1d1f21'"
      dmenuEmoji = "BEMOJI_PICKER_CMD=\"dmenu-nk -i -l 20\" bemoji"
      dmenuBluetooth = "DMENU_BLUETOOTH_LAUNCHER=dmenu-nk dmenu-bluetooth --connected-icon \983217 -l 10 -i"
      selectBiaoqing = "sxiv -o ~/images/biaoqing | xargs xclip -selection clipboard -target image/png -i"
      randomBiaoqing = "find ~/images/biaoqing -type f | shuf | head -n 1 | xargs xclip -selection clipboard -target image/png -i"
   in [ -- Essentials <top-level>
        ("M-<Return>", spawn term),
        ("M-S-s", unGrab *> spawn scrot),
        ("<Print>", unGrab *> spawn scrotFull),
        ("M1-<Print>", unGrab *> spawn scrotWindow),
        ("M-C-s", unGrab *> spawn scrotZbar),
        ("M-e", spawn $ term <> "-e nnn"),
        ("M-r", spawn dmenuRun),
        ("M-S-e", spawn dmenuEmoji),
        ("M-S-n", spawn dmenuBluetooth),
        ("M-S-c", spawn xcolor),
        ("M-z", spawn "boomer"),
        -- Memes <M-b>
        ("M-o", spawn randomBiaoqing),
        ("M-S-o", spawn selectBiaoqing),
        ("M-b c", spawn "mpv ~/videos/memes/chad.mp4"),
        ("M-b m", spawn "mpv ~/videos/memes/monke.mp4"),
        ("M-b t", spawn "mpv ~/videos/memes/terry.webm"),
        ("M-b g", spawn "mpv ~/videos/memes/gamblecore.webm"),
        ("M-b h", spawn "mpv ~/videos/memes/chip-stayin-alive.webm"),
        ("M-b u", spawn "mpv ~/videos/memes/gilgamesh.mp4"),
        ("M-b r", spawn "mpv ~/videos/memes/rat-microwave-dance.mp4"),
        ("M-b d", spawn "mpv ~/videos/memes/douapuncteparantezaparanteza.mp4"),
        ("M-v", spawn "mpv ~/videos/memes/Vineboomsoundeffect.m4a"),
        -- Apps <M-a> (mostly)
        ("M-w", kill),
        ("M-a b", spawn "chromium"),
        ("M-a d", spawn "discord"),
        ("M-a s", spawn "spotify"),
        ("M-a n", spawn "neovide"),
        -- Scratchpads <M-s>
        ("M-s k", namedScratchpadAction myScratchpads "keepassxc"),
        ("M-s v", namedScratchpadAction myScratchpads "pwvucontrol"),
        ("M-s c", namedScratchpadAction myScratchpads "carla"),
        -- Window movement
        ("M1-<Tab>", windows W.focusDown),
        ("M1-S-<Tab>", windows W.focusUp),
        ("M-j", windows W.focusDown),
        ("M-k", windows W.focusUp),
        ("M-m", windows W.focusMaster),
        -- Window order
        ("M-S-j", windows W.swapDown),
        ("M-S-k", windows W.swapUp),
        ("M-S-m", windows W.swapMaster),
        -- Window sizing and floating
        ("M-t", withFocused $ windows . W.sink),
        ("M-h", sendMessage Shrink),
        ("M-l", sendMessage Expand),
        ("M-f", toggleFull),
        -- ThreeColumnStable
        ("M-S-h", sendMessage $ MoveWindow Prev True),
        ("M-S-l", sendMessage $ MoveWindow Next True),
        ("M-C-h", sendMessage $ MoveColumn Prev True),
        ("M-C-l", sendMessage $ MoveColumn Next True),
        ("M-M1-h", sendMessage $ RotateColumns Prev True),
        ("M-M1-l", sendMessage $ RotateColumns Next True),
        ("M-S-t", sendMessage TidyColumns),
        -- Layouts
        ("M-;", sendMessage NextLayout),
        -- Heads
        ("M-,", onPrevNeighbour def W.view),
        ("M-.", onPrevNeighbour def W.view),
        ("M-S-.", onPrevNeighbour def W.shift),
        ("M-S-,", onNextNeighbour def W.shift),
        -- Control
        ("M-S-q", io exitSuccess),
        ("M-S-r", spawn "xmonad --restart"),
        -- Media keys
        ("<XF86PowerDown>", spawn "sudo systemctl suspend"),
        ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 5%+"),
        ("<XF86AudioLowerVolume>", spawn "amixer sset Master 5%-"),
        ("<XF86AudioMute>", spawn "amixer sset Master toggle"),
        ("<XF86AudioPlay>", spawn "playerctl play-pause"),
        ("<XF86AudioPause>", spawn "playerctl pause"),
        ("<XF86AudioStop>", spawn "playerctl stop"),
        ("<XF86AudioNext>", spawn "playerctl next"),
        ("<XF86AudioPrev>", spawn "playerctl previous"),
        ("<XF86MonBrightnessUp>", spawn "brightnessctl set +10%"),
        ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
      ]
        ++
        -- mod-[1..9] %! Switch to workspace N
        -- mod-[1..9] %! Switch to workspace N
        -- mod-shift-[1..9] %! Move client to workspace N
        -- mod-shift-[1..9] %! Move client to workspace N
        [ ("M-" <> m <> k, windows $ f i)
          | (i, k) <- zip (XMonad.workspaces conf) (map show [1 .. 9]),
            (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]
        ]

-- Bar config
myStatusBarSpawner :: (Applicative f) => ScreenId -> f StatusBarConfig
myStatusBarSpawner (S s) = do
  pure $
    statusBarProp
      ("xmobar -x " ++ show s)
      (pure $ myXmobarPP (S s))

myXmobarPP :: ScreenId -> PP
myXmobarPP s =
  filterOutWsPP [scratchpadWorkspaceTag] $
    def
      { ppSep = base !! 0x0E $ " • ",
        ppTitleSanitize = ppWindow,
        ppCurrent = wrap " " "" . xmobarBorder "Bottom" (base16Colors !! 0x0D) 1,
        ppHidden = (base !! 0x07) . wrap " " "",
        ppHiddenNoWindows = (base !! 0x03) . wrap " " "",
        ppUrgent = (base !! 0x08) . wrap (base !! 0x0A $ "!") (base !! 0x0A $ "!"),
        ppOrder = \[ws, l, wt, _] -> [ws, l, wt],
        ppExtras = [logTitles formatFocused formatUnfocused]
      }
  where
    ppWindow = xmobarRaw . (\w -> if null w then "" else w) . shorten 30
    formatFocused = wrap (base !! 0x07 $ "[") (base !! 0x07 $ "]") . (base !! 0x0E) . ppWindow
    formatUnfocused = wrap (base !! 0x03 $ "[") (base !! 0x03 $ "]") . (base !! 0x0D) . ppWindow

    base = map (`xmobarColor` "") base16Colors

-- Utils
toggleFull :: X ()
toggleFull =
  withFocused
    ( \windowId -> do
        floats <- gets (W.floating . windowset)
        if windowId `M.member` floats
          then do
            withFocused $ toggleBorder
            withFocused $ windows . W.sink
          else do
            withFocused $ toggleBorder
            withFocused $ windows . flip W.float (W.RationalRect 0 0 1 1)
    )

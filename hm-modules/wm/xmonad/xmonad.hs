import System.Exit
import qualified Data.Map as M

import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders

import qualified XMonad.StackSet as W

import Graphics.X11.ExtraTypes.XF86

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) toggleStrutsKey
     $ myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m .|. shiftMask, xK_b)

base16Colors :: [String]
base16Colors =
    ["#1d1f21", "#282a2e", "#373b41", "#969896", "#b4b7b4", "#c5c8c6",
     "#e0e0e0", "#ffffff", "#cc6666", "#de935f", "#f0c674", "#b5bd68",
     "#8abeb7", "#81a2be", "#b294bb", "#a3685a"]

myConfig = def
    { workspaces  = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    , terminal    = "alacritty"
    , modMask     = mod4Mask
    , keys        = myKeys
    , layoutHook  = smartBorders . avoidStruts . renamed [KeepWordsRight 1]
                  . spacingRaw False (Border 2 0 0 0) True (Border 0 0 0 0) False
                  $ myLayoutHook
    , startupHook = myStartupHook

    , normalBorderColor  = base16Colors !! 0x2
    , focusedBorderColor = base16Colors !! 0xE
    }

myLayoutHook = tiled ||| Full
  where
    tiled = renamed [Replace "MasterStack"] $ Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myStartupHook :: X ()
myStartupHook = do
  -- spawnOnce "trayer --edge top --align right --SetDockType true \
  --           \--SetPartialStrut true --expand true --width 3 \
  --           \--transparent true --alpha 0 --tint 0xFF000000 --height 17"
  spawnOnce "keepassxc"

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "firefox" --> doShift "2"
    , className =? "discord" --> doShift "3"
    , className =? "spotify" --> doShift "5"
    , className =? "Gimp"    --> doShift "7" <+> doFloat
    , isDialog               --> doFloat
    ]

dmenuParams = "-fn 'FiraCode Nerd Font Mono-12' -nb '#000000' -nf '#ffffff' -sb '#b294bb' -sf '#1d1f21'"
dmenuRunCmd = "dmenu_run" <> dmenuParams
dmenuBluetoothCmd = "DMENU_BLUETOOTH_LAUNCHER=dmenu-nk dmenu-bluetooth --connected-icon \983217 -l 10 -i"
dmenuEmojiCmd = "BEMOJI_PICKER_CMD=\"dmenu-nk -i -l 20\" bemoji"

scrotCmd = "sleep 0.2; scrot -zfs -F '/tmp/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f && rm $f'"
xcolorCmd = "xcolor | xclip -selection clipboard -in"

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask,               xK_r     ), spawn dmenuRunCmd)
    , ((modMask .|. shiftMask, xK_n     ), spawn dmenuBluetoothCmd)
    , ((modMask,               xK_e     ), spawn dmenuEmojiCmd)
    , ((modMask .|. shiftMask, xK_s     ), unGrab *> spawn scrotCmd)
    , ((modMask .|. shiftMask, xK_c     ), spawn xcolorCmd)
    , ((modMask,               xK_w     ), kill)
    , ((modMask,               xK_b     ), spawn "firefox")
    , ((modMask,               xK_d     ), spawn "discord")
    , ((modMask,               xK_n     ), spawn "neovide")

    -- meme
    , ((modMask .|. mod1Mask,  xK_s     ), spawn "xscreensaver-command -select 252")
    , ((modMask .|. mod1Mask,  xK_n     ), spawn "mpv ~/videos/memes/chad.mp4")
    , ((modMask .|. mod1Mask,  xK_m     ), spawn "mpv ~/videos/memes/monke.mp4")
    , ((modMask .|. mod1Mask,  xK_t     ), spawn "mpv ~/videos/memes/terry.webm")
    , ((modMask .|. mod1Mask,  xK_g     ), spawn "mpv ~/videos/memes/gamblecore.webm")
    , ((modMask .|. mod1Mask,  xK_h     ), spawn "mpv ~/videos/memes/chip-stayin-alive.webm")

    -- media keys
    , ((noModMask, xF86XK_PowerDown        ), spawn "sudo systemctl suspend")
    , ((noModMask, xF86XK_AudioRaiseVolume ), spawn "amixer sset Master 5%+")
    , ((noModMask, xF86XK_AudioLowerVolume ), spawn "amixer sset Master 5%-")
    , ((noModMask, xF86XK_AudioMute        ), spawn "amixer sset Master toggle")
    , ((noModMask, xF86XK_MonBrightnessUp  ), spawn "brightnessctl set +10%")
    , ((noModMask, xF86XK_MonBrightnessDown), spawn "brightnessctl set 10%-")

    , ((modMask,               xK_semicolon), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_semicolon), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default
    , ((modMask,               xK_p     ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown  ) -- %! Move focus to the next window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp    ) -- %! Move focus to the previous window
    , ((modMask,               xK_j     ), windows W.focusDown  ) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp    ) -- %! Move focus to the previous window
    , ((modMask,               xK_m     ), windows W.focusMaster) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask .|. shiftMask, xK_m     ), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io exitSuccess) -- %! Quit xmonad
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = base !! 0x0E $ " • "
    , ppTitleSanitize   = ppWindow
    , ppCurrent         = wrap " " "" . xmobarBorder "Bottom" (base16Colors !! 0x0D) 1
    , ppHidden          = (base !! 0x07) . wrap " " ""
    , ppHiddenNoWindows = (base !! 0x03) . wrap " " ""
    , ppUrgent          = (base !! 0x08) . wrap (base !! 0x0A $ "!") (base !! 0x0A $ "!")
    , ppOrder           = \[ws, l, wt, _] -> [ws, l, wt]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    ppWindow = xmobarRaw . (\w -> if null w then "" else w) . shorten 30
    formatFocused   = wrap (base !! 0x07 $ "[") (base !! 0x07 $ "]") . (base !! 0x0E) . ppWindow
    formatUnfocused = wrap (base !! 0x03 $ "[") (base !! 0x03 $ "]") . (base !! 0x0D) . ppWindow

    base = map (\color -> xmobarColor color "") base16Colors

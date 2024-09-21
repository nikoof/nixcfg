import XMonad
import XMonad.Util.EZConfig

main :: IO ()
main = xmonad $ def
    { modMask = mod4Mask
    }
    `additionalKeysP`
    [ ("M-b" , spawn "firefox" )
    ]

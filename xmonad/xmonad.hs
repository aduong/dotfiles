import XMonad
import XMonad.Config.Xfce
import XMonad.Hooks.SetWMName

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Column
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders

main = xmonad $ docks xfceConfig
  { modMask = mod4Mask
  , manageHook = (className =? "Xfce4-notifyd" --> doIgnore) <+> fullscreenManageHook <+> manageHook xfceConfig
  , layoutHook = smartBorders $ fullscreenFull $ avoidStruts $ (layoutHook xfceConfig ||| Column 1 ||| Mirror (Column 1))
  , handleEventHook = fullscreenEventHook
  , focusFollowsMouse = False
  , clickJustFocuses = False
  , terminal = "xfce4-terminal"
  }


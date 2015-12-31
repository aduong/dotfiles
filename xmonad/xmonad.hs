import Data.Functor((<$>))
import Data.List(intercalate)
import qualified XMonad.StackSet as W
import System.IO(Handle)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Util.Run

mkTempDir :: String -> IO String
mkTempDir template = rstrip <$> runProcessWithInput "mktemp" ["--directory", template] ""

mkfifo :: MonadIO m => FilePath -> m ()
mkfifo path = safeSpawn "/usr/bin/mkfifo" [ path ]

xmobarAddPipeReader :: FilePath -> String -> String
xmobarAddPipeReader pipePath alias = " --add-command='[Run PipeReader \"" ++ pipePath ++ "\" \"" ++ alias ++ "\"]' "

spawnPipeXMobar :: MonadIO m => [String] -> m Handle
spawnPipeXMobar args = spawnPipe $ concat $ "xmobar " : args

paCtl = "/usr/local/bin/pulseaudio-ctl.sh"
backlightCtl = "/usr/local/bin/xbacklight-ctl.sh"

paCmd :: MonadIO m => FilePath -> String -> m ()
paCmd pipePath cmd = safeSpawn paCtl [cmd, pipePath]

backlightCmd :: MonadIO m => FilePath -> String -> m ()
backlightCmd pipePath cmd = safeSpawn backlightCtl [cmd, pipePath]

lstrip :: String -> String
lstrip s =
    case s of
      [] -> []
      (x:xs) -> if elem x "\t\r\n"
                then lstrip xs
                else s

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

setupXMobar :: IO (Handle, String -> X (), String -> X ())
setupXMobar = do
  pipeDir <- mkTempDir "/tmp/xmonad.XXXXXX"
  brightPipe <- return $ pipeDir ++ "/xmobar_bright"
  volumePipe <- return $ pipeDir ++ "/xmobar_volume"
  mkfifo brightPipe
  mkfifo volumePipe

  xmproc <- spawnPipeXMobar [ xmobarAddPipeReader brightPipe "BrightPipe"
                            , xmobarAddPipeReader volumePipe "VolumePipe"]

  paCmd volumePipe "update"
  backlightCmd brightPipe "update"

  return (xmproc, paCmd volumePipe, backlightCmd brightPipe)

sleep :: MonadIO m => m ()
sleep = safeSpawn "/usr/bin/dbus-send"
        ["--system"
        , "--print-reply"
        , "--dest=org.freedesktop.login1"
        , "/org/freedesktop/login1"
        , "org.freedesktop.login1.Manager.Suspend"
        , "boolean:true"
        ]

lockScreen :: MonadIO m => m ()
lockScreen = safeSpawn "/usr/bin/gnome-screensaver-command" ["--lock"]

mediaCmd :: MonadIO m => String ->  m ()
mediaCmd cmd = safeSpawn "/usr/bin/dbus-send"
               [ "--print-reply"
               , "--dest=org.mpris.MediaPlayer2.spotify"
               , "/org/mpris/MediaPlayer2"
               , "org.mpris.MediaPlayer2.Player." ++ cmd
               ]

main = do
  (xmobarProc, volumeCmd, backlightCmd) <- setupXMobar

  xmonad $ defaultConfig
       { modMask = mod4Mask
       , startupHook = setWMName "LG3D"
       , manageHook = fullscreenManageHook <+> manageDocks <+> manageHook defaultConfig
       , layoutHook = noBorders $ fullscreenFull $ avoidStruts $ layoutHook defaultConfig
       , handleEventHook = fullscreenEventHook
       , logHook = fadeInactiveLogHook 0.8 >> dynamicLogWithPP xmobarPP
                   { ppOutput = hPutStrLn xmobarProc
                   , ppLayout = const ""
                   , ppTitle = xmobarColor "orange" "" . shorten 50
                   }
       }
       `additionalKeysP`
       [ ("M1-<Tab>", windows W.focusDown)
       , ("M1-S-<Tab>", windows W.focusUp)
       , ("M1-<F4>", kill)
       , ("M-S-l", lockScreen)
       , ("M-p", spawn "$(yeganesh -x)")
       , ("M-<Left>", prevWS)
       , ("M-<Right>", nextWS)
       , ("M-S-<Left>", shiftToPrev >> prevWS)
       , ("M-S-<Right>", shiftToNext >> nextWS)
       , ("<XF86Sleep>", lockScreen >> sleep)
       , ("<XF86AudioMute>", volumeCmd "toggle")
       , ("<XF86AudioRaiseVolume>", volumeCmd "increase")
       , ("<XF86AudioLowerVolume>", volumeCmd "decrease")
       , ("<XF86MonBrightnessUp>", backlightCmd "increase")
       , ("<XF86MonBrightnessDown>", backlightCmd "decrease")
       , ("<XF86AudioPlay>", mediaCmd "PlayPause")
       , ("<XF86AudioNext>", mediaCmd "Next")
       , ("<XF86AudioPrev>", mediaCmd "Previous")
       ]

import Control.Monad
import Codec.Binary.UTF8.String (encodeString)
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe (isNothing, isJust, catMaybes, fromMaybe)
import Data.Ratio ((%))
import Data.Function
import Data.Monoid
import System.Exit
import System.IO
import System.Process
import System.Posix.Process (executeFile)
import System.Posix.Types (ProcessID)
import Text.Printf
import Text.Regex

import XMonad hiding ((|||))
import qualified XMonad.StackSet as W
import XMonad.Util.ExtensibleState as XS
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste
import XMonad.Util.Run
import qualified XMonad.Util.Themes as Theme
import XMonad.Util.WorkspaceCompare

import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace

import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.Commands
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Actions.GridSelect
import XMonad.Actions.Navigation2D
import qualified XMonad.Actions.Search as S
import XMonad.Actions.Submap
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Actions.WindowMenu
import XMonad.Actions.WithAll (sinkAll, killAll)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.UrgencyHook

import XMonad.Config.Gnome
import XMonad.Layout.WindowNavigation
import XMonad.Layout.IM
import XMonad.Layout.Mosaic
import XMonad.Layout.SimpleFloat
import XMonad.Layout.AutoMaster
import XMonad.Layout.DragPane
import qualified XMonad.Layout.HintedGrid as HintedGrid
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Master
import XMonad.Layout.Maximize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.DraggingVisualizer
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeWindows

-- Theme {{{
-- Color names are easier to remember:
colorOrange         = "#FD971F"
colorDarkGray       = "#1B1D1E"
colorPink           = "#F92672"
colorGreen          = "#A6E22E"
colorBlue           = "#66D9EF"
colorYellow         = "#E6DB74"
colorWhite          = "#CCCCC6"
colorNormalBorder   = "#CCCCCp6"
colorFocusedBorder  = "#FF0000"
barFont  = "terminus"
barXFont = "inconsolata:size=12"
xftFont = "xft: inconsolata-14"
--}}}

main :: IO ()
main = do
  checkTopicConfig myTopicNames myTopicConfig
  init <- spawn "~/.xmonad/startxmonad"
  xmonad =<< statusBar cmd myXmobarPP toggleStrutsKey config
  where
    -- cmd = "bash -c \"tee >(xmobar -x0) | xmobar -x1\""
    cmd = "xmobar ~/.xmonad/xmobar.hs -x0"
    toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
    config = ewmh $ withNavigation2DConfig myNavigation2DConfig $
             withUrgencyHook NoUrgencyHook $ defaultConfig {
               terminal             = "urxvtc"
               , modMask            = mod4Mask
               , workspaces         = myTopicNames
               , borderWidth        = 3
               , layoutHook         = layoutHook'
               , focusedBorderColor = colorFocusedBorder
               , logHook            = fadeWindowsLogHook myFadeHook
               , handleEventHook    = fadeWindowsEventHook
               , manageHook         = manageHook'
               } `additionalKeysP` myKeys

myGSConfig = ["xrandr --output VGA1 --primary", "disper -e -r 1366x768,1280x960 -t top"
              , "urxvtd -q -f -o", "emacsclient -c", "emacs -daemon"
              , "xrandr --output LVDS1 --off", "google-chrome-stable --incognito"]

myCommands = [
  -- ("getmail", namedScratchpadAction scratchpads "getmail")
  -- , ("wallpaper", safeSpawn "change-wallpaper" [])
             ]

myXmobarPP = defaultPP {
  ppCurrent = xmobarColor "#429942" "" . wrap "[" "]"
  , ppVisible = xmobarColor "#429942" ""
  , ppHidden = xmobarColor "#C98F0A" ""
  , ppHiddenNoWindows = const ""
  , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "" ""
  , ppLayout = xmobarColor "#C9A34E" ""
               . shorten 10
               . flip (subRegex (mkRegex "ReflectX")) "[|]"
               . flip (subRegex (mkRegex "ReflectY")) "[-]"
               . flip (subRegex (mkRegex "Mirror")) "[+]"
  , ppTitle = xmobarColor "#C9A34E" "" . shorten 24
  , ppSep = xmobarColor "#429942" "" " | "
  -- , ppOrder  = \(ws:l:t:exs) -> []++exs
  , ppSort   = fmap (namedScratchpadFilterOutWorkspace.) (ppSort byorgeyPP)
  }

myFadeHook = composeAll [isUnfocused --> transparency 0.1
                        ,                transparency 0.1
                        ]

manageHook' :: ManageHook
manageHook' = composeAll . concat $
    [
      [isDialog --> doFloat]
    , [isFullscreen --> doFullFloat]
    , [(className =? i <||> resource =? i) --> doIgnore | i <- myIgnores]
    , [(className =? x <||> title =? x <||> resource =? x) --> doSink | x <- mySinks]
    , [(className =? x <||> title =? x <||> resource =? x) --> doFullFloat | x <- myFullscreens]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "web" | x <- myWeb]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "code" | x <- myCode]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "doc" | x <- myDoc]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "code2" | x <- myCode2]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "chat" | x <- myChat]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "media" | x <- myMedia]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "gimp" | x <- myGimp]
    , [title =? t --> doCenterFloat | t <- myTFloats]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [name =? n --> doCenterFloat | n <- myNFloats]
    , [manageDocks , namedScratchpadManageHook scratchpads ]
    , [resource =? r --> doCenterFloat | r <- myRFloats]
    , [role =? r --> doCenterFloat | r <- myRolesF]
    ]
    where
      name = stringProperty "WM_NAME"
      role = stringProperty "WM_WINDOW_ROLE"
      -- popup = stringProperty "WM_WINDOW_ROLE"
      -- Hook used to shift windows without focusing them
      doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
      -- Hook used to push floating windows back into the layout
      -- This is used for gimp windwos to force them into a layout.
      doSink = ask >>= \w -> liftX (reveal w) >> doF (W.sink w)
      -- Float dialogs, Download windows and Save dialogs
      myCFloats = ["Sysinfo", "XMessage", "Smplayer"
                  ,"MPlayer", "nemo"
                  , "Toplevel", "Pcmanfm", "goldendict"
                  , "Xmessage","XFontSel","Downloads"
                  ,"Nm-connection-editor", "Pidgin"
                  , "stardict", "StarDict"]

      myTFloats = ["Downloads", "Save As..."]
      myRFloats = ["Dialog"]
      myRolesF = ["pop-up"]
      myNFloats   = ["bashrun","Google Chrome Options","Chromium Options"
                    , "Hangouts", "stardict", "StarDict"
                    , "System Settings", "Library", "Firefox Preference"]
      myDoc = ["VirtualBox"]
      mySinks = ["gimp"]
      -- Ignore gnome leftovers
      myIgnores = ["Unity-2d-panel", "Unity-2d-launcher", "desktop_window", "kdesktop",
                   "desktop","desktop_window","notify-osd","stalonetray","trayer"]
      -- Run VLC, firefox and VLC on fullscreen
      myFullscreens = ["vlc", "Image Viewer"]

      -- Define default workspaces for some programs
      myWeb = ["Firefox-bin", "Firefox",  "Firefox Web Browser"
              , "opera", "Opera", "Chromium"]
      myChat = ["Pidgin Internet Messenger", "Buddy List"
               , "skype", "skype-wrapper", "Skype", "Conky"]
      -- myCode = ["geany", "Emacs24", "Gvim", "emacs", "emacsclient"]
      myCode = ["geany", "Gvim", "emacs"]
      myCode2 = ["gnome-terminal", "eclipse", "Eclipse", "jetbrains-idea-ce", "jetbrains-idea"]
      myGimp = ["Gimp", "GIMP Image Editor"]
      myMedia = ["Rhythmbox","Spotify","Boxee","Trine"]


myNavigation2DConfig = defaultNavigation2DConfig {
  layoutNavigation   = [("Full", centerNavigation)]
  , unmappedWindowRect = [("Full", singleWindowRect)]
  }

layoutHook' = onWorkspaces ["gimp"] gimpLayout $
              onWorkspaces ["chat"] imLayout $
              customLayout
  where
    customLayout = avoidStruts $
                   -- configurableNavigation (navigateColor "#00aa00") $
                   -- mkToggle1 TABBED $
                   mkToggle1 NBFULL $
                   mkToggle1 REFLECTX $
                   mkToggle1 REFLECTY $
                   mkToggle1 MIRROR $
                   mkToggle1 NOBORDERS $
                   smartBorders $
                   -- Full
                   mosaic 1.5 [7,5,2]
                   ||| simpleFloat
                   ||| dragPane Horizontal 0.1 0.3
                   ||| ResizableTall 1 (3/100) (1/2) []
                   -- ||| autoMaster 1 (1/20) (Mag.magnifier Grid)
                   ||| HintedGrid.GridRatio (4/3) False

    gimpLayout = avoidStruts $ withIM (0.11) (Role "gimp-toolbox") $
                 reflectHoriz $
                 withIM (0.15) (Role "gimp-dock") customLayout

    imLayout = avoidStruts $ withIM (1%5) (And (ClassName "Pidgin") (Role "buddy_list")) $
               reflectHoriz $
               withIM (1%7) (ClassName "Conky") customLayout



myKeys =
    [ ("M-" ++ m ++ [k], f i)
        | (i, k) <- zip myTopicNames "1234567890-="
        , (f, m) <- [ (switchTopic myTopicConfig, "")
                    , (windows . liftM2 (.) W.view W.shift, "S-")
                    ]
    ]
    ++
    [ ("C-; " ++ m ++ [k], f i)
        | (i, k) <- zip myTopicNames "asdfghjk"
        , (f, m) <- [ (switchTopic myTopicConfig, "")
                    , (windows . liftM2 (.) W.view W.shift, "S-")
                    ]
    ]
    ++
    [("M-" ++ m ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
        | (k, sc) <- zip ["w", "e", "r"] [0..]
        , (f, m) <- [(W.view, ""), (liftM2 (.) W.view W.shift, "S-")]
    ]
    ++
    [
      ("M-S-q", io (exitWith ExitSuccess))
      -- ("M-S-q", spawn "gnome-session-quit")
      -- ("M-S-q", namedScratchpadAction scratchpads "reboot")
    , ("M-q", namedScratchpadAction scratchpads "shutdown")
    , ("M-S-r", spawn "ghc -e ':m +XMonad Control.Monad System.Exit' -e 'flip unless exitFailure =<< recompile False' && xmonad --restart")
    , ("M-S-c", kill)

    -- , ("<Print>", spawn "import /tmp/screen.jpg")
    -- , ("C-<Print>", spawn "import -window root /tmp/screen.jpg")
    -- , ("M-<Return>", spawn "urxvtc -e tmux attach" >> sendMessage (JumpToLayout "Mosaic"))
    , ("M-<Return>", windows W.swapMaster)
    , ("M-S-<Return>", spawn "urxvtc")
    , ("C-; o o", spawnSelected defaultGSConfig myGSConfig)
    , ("C-; o d", spawn "pcmanfm")
    , ("C-; o f", placeFocused $ withGaps (22, 0, 0, 0) $ smart (0.5,0.5))
    , ("C-; o k", kill)
    , ("C-; o i", spawn "xcalib -i -a")
    , ("C-; o l", spawn "xscreensaver-command -lock")
    -- , ("C-; o d", spawn "xkill")
    , ("C-; o q", spawn "systemctl suspend")
    -- , ("<XF86AudioNext>", spawn "mpc_seek forward")
    -- , ("<XF86AudioPrev>", spawn "mpc_seek backward")
    , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 9+")
    , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 9-")

    , ("M--", spawn "amixer sset Master 9-")
    , ("M-=", spawn "amixer sset Master 9+")
    , ("M-S--", spawn "amixer sset Master 1-")
    , ("M-S-=", spawn "amixer sset Master 1+")

    , ("M-<F1>", spawn "ksnapshot --region")  --抓部分
    , ("M-<F12>", spawn "ksnapshot -c")  --抓全屏

    , ("<XF86AudioMute>", spawn "amixer set Master mute")
    -- , ("<XF86AudioPlay>", spawn "mpc toggle")
    , ("<XF86Eject>", spawn "eject")
    , ("M-S-a", sendMessage Taller)
    , ("M-S-z", sendMessage Wider)

    -- window management
    , ("M-s", withFocused $ windows . W.sink)
    , ("C-; o b", windowPromptBring myXPConfig)
    , ("C-; o c", banishScreen LowerRight)
    , ("M-<Tab>", cycleRecentWS [xK_Super_L] xK_Tab xK_Tab)
    , ("M1-<Tab>", windows W.focusDown)
    , ("M1-<F3>", kill)
    , ("M-n", doTo Next EmptyWS getSortByIndex (windows . liftM2 (.) W.view W.shift))
    , ("M-<Space>", sendMessage NextLayout)
    , ("M-o", sendMessage Expand)
    , ("M-i", sendMessage Shrink)
    , ("M-,", sendMessage (IncMasterN 1))
    , ("M-.", sendMessage (IncMasterN (-1)))
    , ("C-; o s", sinkAll)
    , ("M-y", focusUrgent)
    , ("M-;", switchLayer)
    , ("M-h", windowGo L True)
    , ("M-j", windowGo D True)
    , ("M-k", windowGo U True)
    , ("M-l", windowGo R True)
    , ("M-S-<L>", withFocused (keysResizeWindow (-30,0) (0,0))) --shrink float at right
    , ("M-S-<R>", withFocused (keysResizeWindow (30,0) (0,0))) --expand float at right
    , ("M-S-<D>", withFocused (keysResizeWindow (0,30) (0,0))) --expand float at bottom
    , ("M-S-<U>", withFocused (keysResizeWindow (0,-30) (0,0))) --shrink float at bottom
    , ("M-C-<L>", withFocused (keysResizeWindow (30,0) (1,0))) --expand float at left
    , ("M-C-<R>", withFocused (keysResizeWindow (-30,0) (1,0))) --shrink float at left
    , ("M-C-<U>", withFocused (keysResizeWindow (0,30) (0,1))) --expand float at top
    , ("M-C-<D>", withFocused (keysResizeWindow (0,-30) (0,1))) --shrink float at top
    , ("M-<L>", withFocused (keysMoveWindow (-30,0)))
    , ("M-<R>", withFocused (keysMoveWindow (30,0)))
    , ("M-<U>", withFocused (keysMoveWindow (0,-30)))
    , ("M-<D>", withFocused (keysMoveWindow (0,30)))
    , ("C-; <L>", withFocused $ snapMove L Nothing)
    , ("C-; <R>", withFocused $ snapMove R Nothing)
    , ("C-; <U>", withFocused $ snapMove U Nothing)
    , ("C-; <D>", withFocused $ snapMove D Nothing)

    -- dynamic workspace
    , ("C-; w n", addWorkspacePrompt myXPConfig)
    , ("C-; w k", removeWorkspace)
    , ("C-; w r", killAll >> removeWorkspace)

    -- Volume
    -- , ("C-; 9", spawn "change_volume down")
    -- , ("C-; 0", spawn "change_volume up")
    -- , ("C-; m", spawn "change_volume toggle")

    -- preferred cui programs

    , ("C-; C-;", pasteChar controlMask ';')
    , ("C-' s", namedScratchpadAction scratchpads "stardict")
    , ("C-' d", namedScratchpadAction scratchpads "g-dict")
    , ("C-' k", namedScratchpadAction scratchpads "g-keep")
    , ("C-' m", namedScratchpadAction scratchpads "g-mail")
    , ("C-' t", namedScratchpadAction scratchpads "g-task")
    , ("C-' e", namedScratchpadAction scratchpads "emacs")
    , ("C-' z", namedScratchpadAction scratchpads "zsh")

    , ("C-' c", namedScratchpadAction scratchpads "g-calendar")
    , ("C-' ' g", namedScratchpadAction scratchpads "ghci")
    , ("C-' ' l", namedScratchpadAction scratchpads "lua")
    , ("C-' ' c", namedScratchpadAction scratchpads "scala")
    , ("C-' ' q", namedScratchpadAction scratchpads "swipl")
    , ("C-' ' o", namedScratchpadAction scratchpads "ocaml")
    , ("C-' ' p", namedScratchpadAction scratchpads "ipython")
    , ("C-' ' r", namedScratchpadAction scratchpads "pry")
    , ("C-' ' s", namedScratchpadAction scratchpads "gst")
    , ("C-' ' j", namedScratchpadAction scratchpads "node")
    , ("C-' ' f", namedScratchpadAction scratchpads "coffee")
    , ("C-' ' a", namedScratchpadAction scratchpads "alsamixer")
    , ("C-' ' m", namedScratchpadAction scratchpads "ncmpcpp")
    , ("C-' ' h", namedScratchpadAction scratchpads "htop")

    , ("C-; <Space>", sendMessage $ Toggle NBFULL)
    , ("C-; l f", sendMessage $ Toggle NBFULL)
    , ("C-; l x", sendMessage $ Toggle REFLECTX)
    , ("C-; l y", sendMessage $ Toggle REFLECTY)
    , ("C-; l m", sendMessage $ Toggle MIRROR)
    , ("C-; l b", sendMessage $ Toggle NOBORDERS)

    -- prompts
    , ("C-; b", workspacePrompt myXPConfig $ switchTopic myTopicConfig)
    , ("C-; m", workspacePrompt myXPConfig $ windows . W.shift)
    , ("C-; p c", mainCommandPrompt myXPConfig)
    , ("C-; p d", changeDir myXPConfig)
    -- , ("C-; p f", fadePrompt myXPConfig)
    , ("C-; p m", manPrompt myXPConfig)
    -- , ("M-p e", launchApp myXPConfig "evince" ["pdf","ps"])
    -- , ("M-p F", launchApp myXPConfig "feh" ["png","jpg","gif"])
    -- , ("M-p l", launchApp myXPConfig "llpp" ["pdf","ps"])
    -- , ("M-p m", launchApp myXPConfig "mupdf" ["pdf","ps"])
    -- , ("M-p z", launchApp myXPConfig "zathura" ["pdf","ps"])
    , ("C-; p p", runOrRaisePrompt myXPConfig)
    , ("M-p", runOrRaisePrompt myXPConfig)
    ]
    ++  searchBindings
{-
 - Topic
 -}
data TopicItem = TI { topicName :: Topic
                    , topicDir  :: Dir
                    , topicAction :: X ()
                    }

myTopicNames :: [Topic]
myTopicNames = map topicName myTopics

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { topicDirs = M.fromList $ map (\(TI n d _ ) -> (n,d)) myTopics
    , defaultTopicAction = const (return ())
    , defaultTopic = "web"
    , maxTopicHistory = 10
    , topicActions = M.fromList $ map (\(TI n _ a ) -> (n,a)) myTopics
    }

myTopics :: [TopicItem]
myTopics =
    [ TI "web" "" (return ())
    , TI "code" "" (urxvt "/home/poe/.gem/ruby/2.1.0/bin/tmuxinator s workspace")
    -- , TI "code" "" (return ())
    , TI "code2" "" (return ())
    , TI "assets" "" (urxvt "tmux")
    , TI "doc" "Documents/" (return ())
    , TI "chat" "" (return ())
    , TI "gimp" "" (return ())
    , TI "media" "" (return ())
    , TI "dust" "" (return ())
    ]
  where
    urxvt prog = spawn . ("urxvtc -T "++) . ((++) . head $ words prog) . (" -e "++) . (prog++) $ ""

myPromptKeymap = M.union defaultXPKeymap $ M.fromList
                 [
                   ((controlMask, xK_g), quit)
                 , ((controlMask, xK_m), setSuccess True >> setDone True)
                 , ((controlMask, xK_j), setSuccess True >> setDone True)
                 , ((controlMask, xK_h), deleteString Prev)
                 , ((controlMask, xK_f), moveCursor Next)
                 , ((controlMask, xK_b), moveCursor Prev)
                 , ((controlMask, xK_p), moveHistory W.focusDown')
                 , ((controlMask, xK_n), moveHistory W.focusUp')
                 , ((mod1Mask, xK_p), moveHistory W.focusDown')
                 , ((mod1Mask, xK_n), moveHistory W.focusUp')
                 , ((mod1Mask, xK_b), moveWord Prev)
                 , ((mod1Mask, xK_f), moveWord Next)
                 ]

myXPConfig :: XPConfig
myXPConfig =
    defaultXPConfig { font                  = xftFont
                    , bgColor               = colorDarkGray
                    , fgColor               = colorGreen
                    , bgHLight              = colorGreen
                    , fgHLight              = colorDarkGray
                    , promptBorderWidth     = 0
                    , height                = 22
                    , historyFilter         = deleteConsecutive
                    , promptKeymap          = myPromptKeymap
                    }

searchBindings = [ ("M-S-/", S.promptSearch myXPConfig multi) ] ++
                 [ ("M-/ " ++ name, S.promptSearch myXPConfig e) | e@(S.SearchEngine name _) <- engines, length name == 1 ]
  where
    promptSearch (S.SearchEngine _ site)
      = inputPrompt myXPConfig "Search" ?+ \s ->
      (S.search "google-chrome" site s >> viewWeb)
    viewWeb = windows $ W.view "web"

    mk = S.searchEngine
    engines = [
      mk "h" "http://www.haskell.org/hoogle/?q="
      , mk "g" "http://www.google.com/search?num=100&q="
      , mk "d" "http://dict.cn/"
      , mk "b" "http://www.baidu.com/s?wd="
      , mk "w" "http://en.wikipedia.org/wiki/Special:Search?go=Go&search="
      , mk "y" "http://www.youtube.com/results?search_type=search_videos&search_query="
      , mk "m" "https://developer.mozilla.org/en-US/search?q="
      , mk "e" "http://erldocs.com/R15B/mnesia/mnesia.html?search="
      , mk "r" "http://www.ruby-doc.org/search.html?sa=Search&q="
      , mk "p" "http://docs.python.org/search.html?check_keywords=yes&area=default&q="
      , mk "s" "https://scholar.google.de/scholar?q="
      , mk "i" "https://ixquick.com/do/search?q="
      , mk "duck" "http://duckduckgo.com/?q="
      , mk "def" "http://www.google.com/search?q=define:"
      , mk "img" "http://images.google.com/images?q="
      , mk "gh" "https://github.com/search?q="
      , mk "bb" "https://bitbucket.org/repo/all?name="
      , mk "ud" "http://www.urbandictionary.com/define.php?term="
      , mk "rtd" "http://readthedocs.org/search/project/?q="
      , mk "null" "http://nullege.com/codes/search/"
      , mk "sf" "http://sourceforge.net/search/?q="
      , mk "acm" "https://dl.acm.org/results.cfm?query="
      , mk "math" "http://mathworld.wolfram.com/search/?query="
      ]
    multi = S.namedEngine "multi" $ foldr1 (S.!>) engines


scratchpads =
  map f ["cmus", "erl", "ghci", "gst", "node", "swipl", "coffee", "ipython", "zsh"
         , "livescript", "pry", "R", "alsamixer", "htop", "xosview", "ncmpcpp"
         , "scala"] ++
  [ NS "utop" "urxvtc -T utop -e rlwrap utop" (title =? "utop") doTopRightFloat
  , NS "task" "urxvtc -T task -e rlwrap task shell" (title =? "task") doTopRightFloat
  , NS "stardict" "stardict" (title =? "stardict") doTopRightFloat
  , NS "emacs" "urxvtc -T emacsnw -e emacsclient '-nw'" (title =? "emacsnw") doTopRightFloat
  , NS "agenda" "org-agenda" (title =? "Agenda Frame") orgFloat
  , NS "capture" "org-capture" (title =? "Capture Frame") orgFloat
  , NS "shutdown" "urxvtc -T shutdown -e sh -c \"sudo shutdown -h 0\"" (title =? "shutdown") doTopFloat
  , NS "reboot" "urxvtc -T reboot -e sh -c \"sudo reboot\"" (title =? "reboot") doTopFloat
  , NS "getmail" "urxvtc -T getmail -e getmail -r rc0 -r rc1" (title =? "getmail") doTopRightFloat

  , NS "g-dict" "google-chrome-stable --app=http://dict.cn/" (title =? "google-dict") doTopRightFloat
  , NS "g-calendar" "google-chrome-stable --app=https://www.google.com/calendar/render" (title =? "google-calendar") doTopRightFloat
  , NS "g-keep" "google-chrome-stable --app=https://drive.google.com/keep/" (title =? "google-keep") doTopRightFloat
  , NS "g-mail" "google-chrome-stable --app=https://mail.google.com/" (title =? "google-mail") doTopRightFloat
  , NS "g-task" "google-chrome-stable --app=https://issues.schoolshape.com/projects/schoolshape/issues" (title =? "g-task") doTopRightFloat

  ]
  where
    urxvt prog = ("urxvtc -T "++) . ((++) . head $ words prog) . (" -e "++) . (prog++) $ ""
    f s = NS s (urxvt s) (title =? s) doTopRightFloat
    doSPFloat = customFloating $ W.RationalRect (1/6) (1/6) (4/6) (4/6)
    doTopFloat = customFloating $ W.RationalRect (1/3) 0 (1/3) (1/3)
    doTopLeftFloat = customFloating $ W.RationalRect 0 0 (1/3) (1/3)
    doTopRightFloat = customFloating $ W.RationalRect (2/3) 0 (1/3) (1/3)
    doBottomLeftFloat = customFloating $ W.RationalRect 0 (2/3) (1/3) (1/3)
    doBottomRightFloat = customFloating $ W.RationalRect (2/3) (2/3) (1/3) (1/3)
    doLeftFloat = customFloating $ W.RationalRect 0 0 (1/3) 1
    orgFloat = customFloating $ W.RationalRect (1/2) (1/2) (1/2) (1/2)

data TitledPrompt = TitledPrompt String

instance XPrompt TitledPrompt where
    showXPrompt (TitledPrompt t)  = t ++ ": "
    commandToComplete _ c   = c
    nextCompletion    _     = getNextCompletion

mkCommandPrompt :: XPConfig -> [(String, X ())] -> X ()
mkCommandPrompt xpc cs = do
    mkXPrompt (TitledPrompt "Command") xpc compl $ \i -> whenJust (find ((==i) . fst) cs) snd
  where
    compl s = return . filter (searchPredicate xpc s) . map fst $ cs

mainCommandPrompt xpc = do
  defs <- defaultCommands
  mkCommandPrompt xpc $ nubBy ((==) `on` fst) $ myCommands ++ defs

getFilesWithExt :: [String] -> String -> IO [String]
getFilesWithExt exts s = fmap lines $ runProcessWithInput "sh" [] ("ls -d -- " ++ s ++ "*/ " ++ s ++ "*." ++ f ++ "\n")
  where
    f = if length exts == 1 then head exts else ('{':) . (++"}") $ intercalate "," exts

{- | Get the user's response to a prompt an launch an application using the
   input as command parameters of the application.-}
launchApp :: XPConfig -> String -> [String] -> X ()
launchApp config app exts = mkXPrompt (TitledPrompt app) config (getFilesWithExt exts) $ launch app
  where
    launch :: MonadIO m => String -> String -> m ()
    launch app params = spawn $ app ++ " " ++ completionToCommand (undefined :: Shell) params

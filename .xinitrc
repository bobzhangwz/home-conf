#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

export _JAVA_AWT_WM_NONREPARENTING=1

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

if [ -f $HOME/.Xmodmap ]; then
    /usr/bin/xmodmap $HOME/.Xmodmap
fi

if [ -f $HOME/.Xresources ]; then
    xrdb $HOME/.Xresources
fi

# Start a D-Bus session
# source /etc/X11/xinit/xinitrc.d/30-dbus 
# Start GNOME Keyring
# eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
# You probably need to do this too:
# export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK

xsetroot -cursor_name left_ptr

# xrandr --output VGA1 --primary --right-of LVDS1 --mode 1680x1050 --output LVDS1 --mode 1366x768 &

xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55 &

# exec gnome-session-cinnamon
# exec gnome-session-xmonad    
# exec ck-launch-session xmonad
exec xmonad
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

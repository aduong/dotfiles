#!/usr/bin/env bash

exec &>> ${LOGFILE:-/var/log/configure-logitech-c920-webcam.log}

date
echo "Setting ${DEVNAME} focus and zoom"
env | grep -e '^DEV' -e '^ID'

if ! v4l2-ctl -d "${DEVNAME}" --list-ctrls | grep -q focus; then
  echo 'No focus, exitting early'
  exit
fi

v4l2-ctl -d "${DEVNAME}" --set-ctrl focus_automatic_continuous=0 
v4l2-ctl -d "${DEVNAME}" --set-ctrl focus_absolute=0,zoom_absolute=150

echo "Set ${DEVNAME} focus=0 and zoom=150"

#!/usr/bin/env bash

nohup xmonad --replace > ~/log/xmonad.log &
sleep 1
xfce4-panel -d -r

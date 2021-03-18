#! /bin/bash

# 
#cd /etc/apt/sources.list.d/

sudo apt-get update
sudo apt-get -y install gnome-session gnome-terminal xinit xterm
sudo apt-get update
sudo apt-get -y dist-upgrade
sudo localectl set-locale LANG=en_US.utf8



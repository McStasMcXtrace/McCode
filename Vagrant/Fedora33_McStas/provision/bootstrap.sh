#! /bin/bash

# 
cd /etc/yum.repos.d/


## CentOS 7.4/6.9 and Red Hat (RHEL) 7.4/6.9 users
sudo curl -O http://download.virtualbox.org/virtualbox/rpm/rhel/virtualbox.repo

sudo yum update
sudo dnf -y group install gnome-desktop base-x

sudo dnf -y install https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm



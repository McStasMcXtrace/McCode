#! /bin/bash

# 
cd /etc/yum.repos.d/


## CentOS 7.4/6.9 and Red Hat (RHEL) 7.4/6.9 users
sudo curl -O http://download.virtualbox.org/virtualbox/rpm/rhel/virtualbox.repo

sudo yum update
sudo yum -y groups install "GNOME Desktop"



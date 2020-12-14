#! /bin/bash

# Git clone and get build mcstas 
cd /home/vagrant/McCode
sudo -u vagrant git pull
sudo -u vagrant ./build_rpms_centos7_mcstas 3.0 meta


#! /bin/bash

# Git clone and get build mcstas 
cd /home/vagrant/McCode
sudo -u vagrant git pull
sudo -u vagrant ./build_rpms_mcstas 2.5beta01 meta
rm dist/mcstas-tools-matlab-mcplot*.rpm
sudo rpm -i dist/*.rpm


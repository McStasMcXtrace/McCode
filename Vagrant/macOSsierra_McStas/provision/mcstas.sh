#!/bin/sh

sudo -u vagrant git clone https://github.com/McStasMcXtrace/McCode.git --recurse-submodules 
cd McCode
sudo -u vagrant git pull
sudo -u vagrant ./getdeps_mac
sudo -u vagrant ./build_macos_mcstas 2.5 

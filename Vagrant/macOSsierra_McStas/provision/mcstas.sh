#!/bin/sh

sudo -u vagrant git clone https://github.com/McStasMcXtrace/McCode.git --depth=1 --recurse-submodules 
cd McCode
sudo -u vagrant git pull
sudo -u vagrant ./getdeps_mac
./build_macos_mcstas 2.7
sudo chown -R vagrant:staff dist

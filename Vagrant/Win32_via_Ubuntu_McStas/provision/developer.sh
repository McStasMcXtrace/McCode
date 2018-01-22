#! /bin/bash

# Install basic developer tools 
sudo apt-get -y update
sudo apt-get -y install build-essential flex bison gcc gfortran cmake tofrodos

sudo dpkg --add-architecture i386

sudo apt-get -y install nsis gcc-mingw-w64-i686 gfortran-mingw-w64-i686 libc6-dev wine32

sudo apt-get -y dist-upgrade


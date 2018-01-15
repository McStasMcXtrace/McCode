#! /bin/bash

# Install basic developer tools 
cd /etc/apt/sources.list.d
sudo curl -O http://packages.mccode.org/debian/mccode.list
cd -

sudo apt-get -y update
sudo apt-get -y install build-essential flex bison gcc gfortran cmake tofrodos

sudo dpkg --add-architecture i386

sudo apt-get -y install nsis gcc-mingw-w64-i686 gfortran-mingw-w64-i686 libc6-dev wine32

sudo apt-get -y dist-upgrade

cd /home/vagrant
sudo -u vagrant git clone https://github.com/McStasMcXtrace/McCode.git --depth=1

sudo -u vagrant tar xzf McCode/support/Win32/Wine/dotwine.tgz

# Hack to allow calling wine without errors
ln -s /home/vagrant/.wine /root/.wine
chmod a+x /root

rm /home/vagrant/.wine/dosdevices/z\:
ln -s /home/vagrant /home/vagrant/.wine/dosdevices/z\:

cd McCode
git pull
sudo -u vagrant ./build_windows_mcstas test meta


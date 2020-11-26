#! /bin/bash

cd /home/vagrant
sudo -u vagrant git clone https://github.com/McStasMcXtrace/McCode.git --depth=1 --recurse-submodules 

sudo -u vagrant tar xzf McCode/support/Win32/Wine/dotwine.tgz

# Hack to allow calling wine without errors
ln -s /home/vagrant/.wine /root/.wine
chmod a+rx /root

rm /home/vagrant/.wine/dosdevices/z\:
ln -s /home/vagrant /home/vagrant/.wine/dosdevices/z\:

cd McCode
git pull
sudo -u vagrant ./build_windows_mcstas 2.7 meta


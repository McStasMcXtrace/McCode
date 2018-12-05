#! /bin/bash


# Ensure everything is up to date...
sudo apt-get -y dist-upgrade

# Get McCode
cd /home/vagrant
sudo -u vagrant git clone https://github.com/McStasMcXtrace/McCode.git --depth=1 --recurse-submodules 
cd McCode
sudo -u vagrant git pull
sudo ./getdeps_debian
# Build the debs
sudo -u vagrant ./build_debs_mcstas 2.5
rm dist/mcstas-tools-matlab-mcplot-*
sudo dpkg -i dist/mcstas-*2.5-*.deb
sudo apt-get -y -f install


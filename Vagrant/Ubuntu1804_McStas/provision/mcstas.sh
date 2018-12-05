#! /bin/bash


# Ensure everything is up to date...
sudo apt-get -y dist-upgrade

# Get McCode
cd /home/vagrant
sudo -u vagrant git clone https://github.com/McStasMcXtrace/McCode.git --recurse-submodules -depth=1
cd McCode
sudo -u vagrant git pull

# Build the debs
sudo -u vagrant ./build_debs_mcstas test
rm dist/mcstas-tools-matlab-mcplot-*
sudo dpkg -i dist/mcstas-*test-*.deb
sudo apt-get -y -f install


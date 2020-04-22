#! /bin/bash


# Ensure everything is up to date...
sudo apt-get -y dist-upgrade

# Get McCode
cd /home/vagrant
sudo -u vagrant git clone https://github.com/McStasMcXtrace/McCode.git --depth=1 --recurse-submodules 
cd McCode
sudo ./getdeps_debian
# Build the debs
sudo -u vagrant ./build_debs_mcxtrace 1.5 meta


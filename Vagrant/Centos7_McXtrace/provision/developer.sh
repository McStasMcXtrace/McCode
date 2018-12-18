#! /bin/bash

# Install basic developer tools 

sudo yum update
sudo yum -y upgrade
sudo yum install -y git flex bison rpm-build gcc-gfortran 
sudo yum install -y epel-release
sudo yum update
sudo yum -y upgrade
sudo yum install -y cmake3 perl-Extuils-Makemaker
sudo ln -sf /bin/cmake3 /usr/bin/cmake
sudo ln -sf /bin/cpack3 /usr/bin/cpack




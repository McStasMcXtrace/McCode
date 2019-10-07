#! /bin/bash

# Install basic developer tools 

sudo dnf update
sudo dnf -y upgrade
sudo dnf install -y git flex bison rpm-build gcc-gfortran 
sudo dnf update
sudo dnf -y upgrade
sudo dnf install -y cmake3 perl-Extuils-Makemaker
sudo ln -sf /bin/cmake3 /usr/bin/cmake
sudo ln -sf /bin/cpack3 /usr/bin/cpack

sudo dnf install -y perl-Tk perl-PDL pgplot pgplot-devel gnuplot perl-ExtUtils-F77 perl-PGPLOT bc 
#xorg-x11-server-Xorg gnome-session gdm
#sudo dnf group install gnome-desktop base-x



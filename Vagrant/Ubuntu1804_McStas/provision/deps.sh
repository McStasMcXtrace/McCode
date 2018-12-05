#! /bin/bash

# Install basic developer tools 
cd /etc/apt/sources.list.d
sudo curl -O http://packages.mccode.org/debian/mccode.list
cd -

sudo apt-get -y update

# Perl / PGPLOT
sudo apt-get -y install perl-tk pdl pgplot5 gnuplot-x11 libtk-codetext-perl libpgplot-perl 
# Python / etc.
sudo apt-get -y install python3 python3-pyqt5 python3-ply python3-pyqtgraph python3-numpy python3-matplotlib python3-mpld3 python3-tornado python3-pyqt5.qsci

# Hack to use the "real" PGPLOT rather than Giza
cd /usr/lib/x86_64-linux-gnu
sudo ln -sf ../libpgplot.so.5 libpgplot.so.0
sudo ln -sf ../libcpgplot.so.5 libcpgplot.so.0


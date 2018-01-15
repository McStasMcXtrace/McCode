#! /bin/bash

# Install basic developer tools 
cd /etc/apt/sources.list.d
sudo curl -O http://packages.mccode.org/debian/mccode.list
cd -

sudo apt-get -y update
sudo apt-get -y install build-essential flex bison gcc gfortran cmake python3 python3-pyqt5
sudo apt-get -y install -y perl-tk pdl pgplot5 gnuplot-x11 libtk-codetext-perl libpgplot-perl
sudo apt-get -y install bc python3-ply python3-pyqtgraph python3-numpy python3-matplotlib python3-mpld3 python3-tornado python3-pyqt5.qsci


cd /usr/lib/x86_64-linux-gnu
sudo ln -sf ../libpgplot.so.5 libpgplot.so.0
sudo ln -sf ../libcpgplot.so.5 libcpgplot.so.0

sudo apt-get -y dist-upgrade

cd /home/vagrant
sudo -u vagrant git clone https://github.com/McStasMcXtrace/McCode.git --depth=1

cd McCode
sudo -u vagrant ./build_debs_mcstas test
rm dist/mcstas-tools-matlab-mcplot-*
sudo dpkg -i dist/mcstas-*test-*.deb
sudo apt-get -y -f install


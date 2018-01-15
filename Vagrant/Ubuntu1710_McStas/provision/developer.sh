#! /bin/bash

# Install basic developer tools 
cd /etc/apt/sources.list.d
sudo curl -O http://packages.mccode.org/debian/mccode.list
cd -

sudo apt-get -y update
sudo apt-get -y install build-essential flex bison gcc gfortran cmake python3 python3-pyqt5
sudo apt-get -y install -y perl-tk pdl pgplot5 gnuplot libtk-codetext-perl libpgplot-perl


cd /home/vagrant
sudo -u vagrant git clone https://github.com/McStasMcXtrace/McCode.git --depth=1

cd McCode

sudo -u vagrant ./build_debs_mcstas test
sudo dpkg -i dist/*.deb
sudo apt-get -y -f install

cd /usr/lib/x86_64-linux-gnu
sudo ln -sf ../libpgplot.so.5 libpgplot.so.0
sudo ln -sf ../libcpgplot.so.5 libcpgplot.so.0

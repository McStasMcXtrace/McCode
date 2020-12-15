#!/bin/sh
# Script to facilitate the installation of McStas 2.7 on TrueOS 18.6

sudo pkg install cmake gcc pgplot p5-PGPLOT p5-Tk PDL bash flex bison py36-qt5 py36-yaml py36-ply py36-matplotlib py36-numpy

sudo ln -sf /usr/local/bin/bash /bin

WORK=`pwd`

mkdir -p TMP
cd TMP

fetch http://downloads.mcstas.org/current/unix/mcstas-2.7-UNIX-src.tar.gz

tar xzf mcstas-2.7-UNIX-src.tar.gz

cd mcstas-2.7-UNIX-src/

find . -name \*tar.gz -exec tar xzf \{\} \;

# Core McStas package:
cd mcstas-2.7-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
cd ..

sleep 10

## McStas components:
cd mcstas-comps-2.7-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
cd ..

sleep 10

# McStas Perl commandline tools
cd mcstas-tools-perl-cmdline-2.7-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
cd ..

sleep 10

# McStas manuals
cd mcstas-manuals-2.7-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
cd ..


# McStas Perl gui tools
cd mcstas-tools-perl-2.7-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
cd ..

sleep 10

# McStas Python tool lib
cd mcstas-tools-python-mccodelib-2.7-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McStas Python mcgui
cd mcstas-tools-python-mcgui-2.7-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McStas Python mcrun
cd mcstas-tools-python-mcrun-2.7-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McStas Python mcplot-matplotlib
cd mcstas-tools-python-mcplot-matplotlib-2.7-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..


# McStas Python mcdisplay-webgl
cd mcstas-tools-python-mcdisplay-webgl-2.7-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McStas Python mcdoc
cd mcstas-tools-python-mcdoc-2.7-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

#fetch and install PyQtgraph
fetch https://pkg.freebsd.org/FreeBSD:12:amd64/quarterly/All/py36-pyqtgraph-0.10.0.txz
sudo pkg add py36-pyqtgraph-0.10.0.txz

# PyQtgraph requires a clone of ports
#sudo git clone https://github.com/trueos/trueos-ports /usr/ports/
# ... plus at least one hack
#cd /usr/local/bin
#sudo ln -sf perl5.24.3 perl5.24.4

#cd /usr/port/graphics/py-pyqtgraph
#sudo make install

# McStas Python mcdisplay-pyqgraph
cd mcstas-tools-python-mcdisplay-pyqtgraph-2.7-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McStas Python mcdisplay-pyqgraph
cd mcstas-tools-python-mcplot-pyqtgraph-2.7-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

cd $WORK

# Make this version the system-wide mcstas
sudo /usr/local/mcstas/2.7/bin/postinst set_mccode_default


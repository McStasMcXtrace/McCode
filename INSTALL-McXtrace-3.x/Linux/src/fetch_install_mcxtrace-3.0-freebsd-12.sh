#!/bin/sh
# Script to facilitate the installation of McXtrace 3.0 on TrueOS 18.6
#
# WARNING: While this installs, you may still need to hack various things, e.g. set
# setenv LD_PRELOAD /usr/local/lib/gcc7/libgcc_s.so.1 to come around the error of
# /lib/libgcc_s.so.1: version GCC_4.8.0 required by /usr/local/lib/gcc7/libgfortran.so.4 not found
# Also, the DBUS service needs to be running for the Qt apps to run...
#
sudo pkg install cmake gcc pgplot p5-PGPLOT p5-Tk PDL bash flex bison py36-qt5 py36-yaml py36-ply py36-matplotlib py36-numpy py36-pyqtgraph

sudo ln -sf /usr/local/bin/bash /bin

WORK=`pwd`

mkdir -p TMP
cd TMP

fetch http://downloads.mcxtrace.org/mcxtrace-3.0/unix/mcxtrace-3.0-UNIX-src.tar.gz

tar xzf mcxtrace-3.0-UNIX-src.tar.gz

cd mcxtrace-3.0-UNIX-src/

find . -name \*tar.gz -exec tar xzf \{\} \;

# Core McXtrace package:
cd mcxtrace-3.0-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcxtrace=1
make
sudo make install
cd ..

sleep 10

## McXtrace components:
cd mcxtrace-comps-3.0-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcxtrace=1
make
sudo make install
cd ..

sleep 10

# McXtrace Perl commandline tools
cd mcxtrace-tools-perl-cmdline-3.0-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcxtrace=1
make
sudo make install
cd ..

sleep 10

# McXtrace manuals
cd mcxtrace-manuals-3.0-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcxtrace=1
make
sudo make install
cd ..


# McXtrace Perl gui tools
cd mcxtrace-tools-perl-3.0-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcxtrace=1
make
sudo make install
cd ..

sleep 10

# McXtrace Python tool lib
cd mcxtrace-tools-python-mccodelib-3.0-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcxtrace=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McXtrace Python mcgui
cd mcxtrace-tools-python-mcgui-3.0-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcxtrace=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McXtrace Python mcrun
cd mcxtrace-tools-python-mcrun-3.0-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcxtrace=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McXtrace Python mcplot-matplotlib
cd mcxtrace-tools-python-mcplot-matplotlib-3.0-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcxtrace=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..


# McXtrace Python mcdisplay-webgl
cd mcxtrace-tools-python-mcdisplay-webgl-3.0-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcxtrace=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McXtrace Python mcdoc
cd mcxtrace-tools-python-mcdoc-3.0-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcxtrace=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McXtrace Python mcdisplay-pyqgraph
cd mcxtrace-tools-python-mcdisplay-pyqtgraph-3.0-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcxtrace=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McXtrace Python mcdisplay-pyqgraph
cd mcxtrace-tools-python-mcplot-pyqtgraph-3.0-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcxtrace=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

cd $WORK

# Make this version the system-wide mcxtrace
sudo /usr/local/mcxtrace/3.0/bin/postinst set_mccode_default


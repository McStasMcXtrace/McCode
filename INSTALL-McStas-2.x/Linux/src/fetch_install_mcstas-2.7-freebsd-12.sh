#!/bin/sh
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
# Script to facilitate the installation of McStas 2.7 on TrueOS 18.6
=======
# Script to facilitate the installation of McStas 3.0 on TrueOS 18.6
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh
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

<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
fetch http://downloads.mcstas.org/mcstas-2.7/unix/mcstas-2.7-UNIX-src.tar.gz

tar xzf mcstas-2.7-UNIX-src.tar.gz

cd mcstas-2.7-UNIX-src/
=======
fetch http://downloads.mcstas.org/current/unix/mcstas-3.0-UNIX-src.tar.gz

tar xzf mcstas-3.0-UNIX-src.tar.gz

cd mcstas-3.0-UNIX-src/
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh

find . -name \*tar.gz -exec tar xzf \{\} \;

# Core McStas package:
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
cd mcstas-2.7-src
=======
cd mcstas-3.0-src
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCSTAS=1
make
sudo make install
cd ..

sleep 10

## McStas components:
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
cd mcstas-comps-2.7-src
=======
cd mcstas-comps-3.0-src
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCSTAS=1
make
sudo make install
cd ..

sleep 10

# McStas Perl commandline tools
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
cd mcstas-tools-perl-cmdline-2.7-src
=======
cd mcstas-tools-perl-cmdline-3.0-src
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCSTAS=1
make
sudo make install
cd ..

sleep 10

# McStas manuals
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
cd mcstas-manuals-2.7-src
=======
cd mcstas-manuals-3.0-src
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCSTAS=1
make
sudo make install
cd ..


# McStas Perl gui tools
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
cd mcstas-tools-perl-2.7-src
=======
cd mcstas-tools-perl-3.0-src
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCSTAS=1
make
sudo make install
cd ..

sleep 10

# McStas Python tool lib
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
cd mcstas-tools-python-mccodelib-2.7-src
=======
cd mcstas-tools-python-mccodelib-3.0-src
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCSTAS=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McStas Python mcgui
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
cd mcstas-tools-python-mcgui-2.7-src
=======
cd mcstas-tools-python-mcgui-3.0-src
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCSTAS=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McStas Python mcrun
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
cd mcstas-tools-python-mcrun-2.7-src
=======
cd mcstas-tools-python-mcrun-3.0-src
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCSTAS=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McStas Python mcplot-matplotlib
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
cd mcstas-tools-python-mcplot-matplotlib-2.7-src
=======
cd mcstas-tools-python-mcplot-matplotlib-3.0-src
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCSTAS=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..


# McStas Python mcdisplay-webgl
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
cd mcstas-tools-python-mcdisplay-webgl-2.7-src
=======
cd mcstas-tools-python-mcdisplay-webgl-3.0-src
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCSTAS=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McStas Python mcdoc
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
cd mcstas-tools-python-mcdoc-2.7-src
=======
cd mcstas-tools-python-mcdoc-3.0-src
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCSTAS=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McStas Python mcdisplay-pyqgraph
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
cd mcstas-tools-python-mcdisplay-pyqtgraph-2.7-src
=======
cd mcstas-tools-python-mcdisplay-pyqtgraph-3.0-src
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCSTAS=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

# McStas Python mcdisplay-pyqgraph
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
cd mcstas-tools-python-mcplot-pyqtgraph-2.7-src
=======
cd mcstas-tools-python-mcplot-pyqtgraph-3.0-src
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCSTAS=1
make
sudo make install
sudo ln -sf /usr/local/bin/python3.6 /usr/local/bin/python3
cd ..

cd $WORK

# Make this version the system-wide mcstas
<<<<<<< HEAD:INSTALL-McStas-2.x/Linux/src/fetch_install_mcstas-2.7-freebsd-12.sh
sudo /usr/local/mcstas/2.7/bin/postinst set_mccode_default
=======
sudo /usr/local/mcstas/3.0/bin/postinst set_mccode_default
>>>>>>> e5e92d9ba... Update readme's:INSTALL-McStas/Linux/src/fetch_install_mcstas-3.0-freebsd-12.sh


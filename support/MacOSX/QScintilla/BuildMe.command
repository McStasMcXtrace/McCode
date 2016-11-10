#!/bin/bash
#
# Simple shellscript snippet for building QScintilla for use with McStas on
# Mac OS X with Anaconda 2.2.0 previously installed
#

# First, ensure that we are in the right location
cd `dirname $0`

# Find out if the globally available Python is an Anaconda
PYTHON=`which python`
PYVER=`$PYTHON --version 2>&1`
SUDO="sudo"
echo Your $PYTHON is $PYVER

if [[ $PYVER =~ .*Anaconda.* ]]
then
   echo "Default Python seems to be Anaconda, starting build!"
else
   echo "$PYTHON does not seem to be Anaconda - sorry, exiting!"
   exit -1
fi
# Check location of anaconda via the conda command
CONDA_ROOT=`conda info |grep "root environment"`
if [[ $CONDA_ROOT =~ .*writable.* ]]
then
   echo "You seem to have a user-writable Anaconda, great!"
   SUDO=""
else
   echo "We need write acces for installation, using sudo below..."
fi
# First, install PyQt via conda
echo ---
$SUDO conda install pyqt=4.11.4
echo ---
# Ensure we will be building using the system-given gcc and g++
# This is done via a crazy link-hack to allow the combination of
# default OS X compilers in /usr/bin together with anaconda's 
# qmake... (Which could be overwritten by a simple /usr/bin first
# on the path...
mkdir -p bin
ln -sf /usr/bin/gcc bin/
ln -sf /usr/bin/g++ bin/
export PATH=$PWD/bin:$PATH
echo Unpacking QScintilla code...
tar xzf QScintilla-gpl-2.9.tar.gz
cd QScintilla-gpl-2.9
# Build and install c++ bindings 
echo ---
echo Starting build of the c++ code
echo NOTE: You may be prompted for your password at install time!!
cd Qt4Qt5
qmake -spec macx-g++ qscintilla.pro
sed -ibak 's/-mmacosx-version-min=10.5/-mmacosx-version-min=10.9/g' Makefile
make
$SUDO make install
echo Done building c++ code
echo ---
# Build and install Python bindings 
cd ..
echo Starting build of the Python code
echo NOTE: You may be promptedfor your password at install time!!
cd Python
$PYTHON configure.py 
# Ensure we are building using the system-given gcc and g++
sed -ibak 's/=\ gcc/=\ \/usr\/bin\/gcc/' Makefile
sed -ibak 's/=\ g++/=\ \/usr\/bin\/g++/' Makefile
sed -ibak 's/-mmacosx-version-min=10.5/-mmacosx-version-min=10.9/g' Makefile
make
$SUDO make install
echo DONE building and installing QScintilla!
echo
CONDA_LIB=`conda info |grep "default environment" | cut -f2 -d: | sed s+\ ++g`
CONDA_QTLIBS="$CONDA_LIB/lib/libQt*"
echo A final required step is linking $CONDA_LIB/lib/libQt\* to /usr/lib - please provide your password if needed:
cd /usr/local/lib
sudo ln -sf $CONDA_QTLIBS .
echo ALL done

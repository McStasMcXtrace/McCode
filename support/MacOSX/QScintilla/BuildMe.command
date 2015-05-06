#!/bin/bash
#
# Simple shellscript snippet for building QScintilla for use with McStas on
# Mac OS X with Anaconda 2.2.0 previously installed
#

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
$SUDO conda install pyqt
echo ---
echo Unpacking QScintilla code...
tar xzf QScintilla-gpl-2.9.tar.gz
cd QScintilla-gpl-2.9
# Build and install c++ bindings 
echo ---
echo Starting build of the c++ code
echo NOTE: You may be prompted for your password at install time!!
cd Qt4Qt5
qmake -spec macx-g++ 
# Ensure we are building using the system-given gcc and g++
sed -ibak 's/=\ gcc/=\ \/usr\/bin\/gcc/' Makefile
sed -ibak 's/=\ g++/=\ \/usr\/bin\/g++/' Makefile
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
make
$SUDO make install
echo DONE building and installing QScintilla!
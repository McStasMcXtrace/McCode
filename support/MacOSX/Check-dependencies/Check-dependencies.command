#!/bin/bash

# Check if Xcode commandline tools is installed

# xcode-select -p to check, otherwise
xcode-select -p
rc=$?; 
if [[ $rc != 0 ]]; 
then 
    osascript -e "tell app \"System Events\" to display dialog \"Xcode commandline tools not installed, spawning installation. Please rerun this tool after installation completes\""
    xcode-select --install
    exit
else
    echo Xcode commandline tools is installed!
    XCODE=1
fi

# Check if Python3 is installed and is anaconda
PYTHON=`which python3`
PYVER=`$PYTHON --version 2>&1`
SUDO="sudo"
echo Your $PYTHON is $PYVER

if [[ $PYVER =~ .*Anaconda.* ]]
then
   echo "Default Python seems to be Anaconda, excellent!"
   # Check location of anaconda via the conda command
   CONDA_ROOT=`conda info |grep "root environment"`
   if [[ $CONDA_ROOT =~ .*writable.* ]]
   then
       echo "You seem to have a user-writable Anaconda, great!"
       SUDO=""
   else
       echo "We need write acces for installation, using sudo below..."
   fi
   $SUDO conda config --add channels conda-forge
   $SUDO conda install qscintilla2 pyqtgraph yaml
else
   osascript -e "tell app \"System Events\" to display dialog \"Your current Python3 is not Anaconda - Please download and install Anaconda Python3 and rerun this tool after installation completes\""
   open https://www.continuum.io/downloads#osx
   exit
fi





#  - and request user to come back for further dependency checks

# if -e /Applications/Utilities/X11.app ...


# Offer to get gcc from hpc.sourceforge.net

# Offer to spawn (c-only) build of openmpi-2.0

# Python tool dependencies:

# Is the default Python3 an anaconda?
# Is qscintilla2 installed?
# (Good news is that we can get it using conda config --add channels conda-forge )
# Perl tool dependencies:
# Check if Xquartz is installed - or simply open /Applications/Utilities/X11.app to get it...
# Figure out which perl version is relevant and download / install SciPDL
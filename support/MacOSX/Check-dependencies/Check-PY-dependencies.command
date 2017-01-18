#!/bin/bash

# Check if Xcode commandline tools is installed

# xcode-select -p to check, otherwise
xcode-select -p
rc=$?; 
if [[ $rc != 0 ]]; 
then 
    osascript -e "tell app \"System Events\" to display dialog \"Xcode commandline tools not installed, spawning installation. \n\n!! Please rerun this tool after installation completes !!\""
    rc1=$?; 
    if [[ $rc1 == 0 ]]; 
    then
	echo
	echo
	echo "******************************************************"
	echo "* Requesting installation of Xcode commandline tools *"
	echo "*                                                    *"
	echo "* Please rerun dependency script after Xcode install *"
	echo "******************************************************"
	echo
	sleep 3
	xcode-select --install
	exit 0
    else
	echo
	echo
	echo "!! Not requesting Xcode cmdline install !!"
	echo
	echo
	sleep 3
        exit 1
    fi
else
    echo Xcode commandline tools is already installed!
fi

# Check if Python3 is installed and is anaconda
PYTHON=`which python3`
PYVER=`$PYTHON --version 2>&1`
SUDO="sudo"
echo Your $PYTHON is $PYVER

# Check if this is an anaconda or miniconda Python 3
if [[ $PYVER =~ .*Anaconda.* || $PYVER =~ .*Continuum.* ]]
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
   echo
   echo
   echo "******************************************************"
   echo "* Requesting installation of Pyhthon dependencies    *"
   echo "*                                                    *"
   echo "* Please follow instructions below                   *"
   echo "******************************************************"
   echo 
   sleep 3
   $SUDO conda config --add channels conda-forge
   $SUDO conda install qscintilla2 pyqtgraph pyaml
   osascript -e "tell app \"System Events\" to display dialog \"As far as this script can tell, all needed dependencies for the Python tools are now installed!\""
else
   osascript -e "tell app \"System Events\" to display dialog \"Your current Python3 is not Anaconda - Please download and install Anaconda Python3.\n\n !! Afterwards please rerun this tool for installing python packages !!\""
   rc1=$?; 
   if [[ $rc1 == 0 ]]; 
   then
       echo
       echo "******************************************************"
       echo "* Spawning browser for downloading Anaconda Python 3 *"
       echo "*                                                    *"
       echo "* Please rerun dependency script after that install *"
       echo "******************************************************"
       echo   
       sleep 3
       open https://www.continuum.io/downloads#osx
       exit 0
   else
       echo
       echo
       echo "!! Not spawning Anaconda website !!"
       echo
       echo
       sleep 3
       exit 1
   fi
fi

osascript -e "tell app \"System Events\" to display dialog \"We recommend that your user $USER takes ownership of /usr/local - do you want to do this? \n\n (Please give your passwd to the sudo command in the terminal...) \""
rc1=$?; 
if [[ $rc1 == 0 ]]; 
then
    echo sudo chown -R $USER:staff /usr/local
    sudo chown -R $USER:staff /usr/local
    mkdir -p /usr/local/bin
else
    echo "Not taking ownership of /usr/local..."
fi

# TODO:
# Offer to get gcc from hpc.sourceforge.net
# Offer to spawn (c-only) build of openmpi-2.0
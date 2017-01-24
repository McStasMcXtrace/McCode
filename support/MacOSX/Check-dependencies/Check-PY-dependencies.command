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

# Look for latest McCode app in current dir...
cd `dirname $0`
echo
echo Locating newest McCode package in $PWD/...
echo
NEWESTAPP=`ls -rt | grep Mc | grep \.app`
echo
echo Seems $NEWESTAPP is where I should go...
echo
export PATH=$PWD/$NEWESTAPP/Contents/Resources/miniconda3/bin:$PATH
echo $PATH
which python3
echo
sleep 1
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
   $SUDO conda install qscintilla2 pyqtgraph pyaml ply -y
   osascript -e "tell app \"System Events\" to display dialog \"As far as this script can tell, all needed dependencies for the Python tools are now installed!\""
else
   osascript -e "tell app \"System Events\" to display dialog \"Your current Python3 is not Anaconda...\n\n !! Do you want me to embed a miniconda into your app?\""
   rc1=$?; 
   if [[ $rc1 == 0 ]]; 
   then
       echo
       echo "******************************************************"
       echo "* Downloading and embeeding miniconda3 in your app   *"
       echo "*                                                    *"
       echo "* Please rerun dependency script after that install *"
       echo "******************************************************"
       echo   
       sleep 3
       curl -O https://repo.continuum.io/miniconda/Miniconda3-latest-MacOSX-x86_64.sh
       chmod a+x Miniconda3-latest-MacOSX-x86_64.sh
       ./Miniconda3-latest-MacOSX-x86_64.sh -b -p $PWD/$NEWESTAPP/Contents/Resources/miniconda3/
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


# Check if the user has a matlab installation - and offer to create a link to the binary in /usr/local
MATLAB=`ls -art /Applications/ |grep MATLAB|wc -l`
if [[ $MATLAB != 0 ]]; 
then
    MATLAB=`ls -art /Applications/ |grep MATLAB|tail -1`
    if [[ -x "/Applications/$MATLAB/bin/matlab" ]];
    then
	osascript -e "tell app \"System Events\" to display dialog \"I found that you have MATLAB installed in /Applications/$MATLAB. \n Do you want that version to be accessible to the tools? \n(I will create a link in /usr/local/bin/matlab, replacing any existing link)\""
	rc1=$?; 
	if [[ $rc1 == 0 ]]; 
	then
	    echo sudo ln -sf /Applications/$MATLAB/bin/matlab /usr/local/bin/matlab
	    sudo ln -sf /Applications/$MATLAB/bin/matlab /usr/local/bin/matlab
	else
	    echo Not installing link to $MATLAB in /usr/local/bin
	fi
	
    fi
fi


# TODO:
# Offer to get gcc from hpc.sourceforge.net
# Offer to spawn (c-only) build of openmpi-2.0
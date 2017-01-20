#!/bin/bash

# Check if Xcode commandline tools is installed

# xcode-select -p to check, otherwise
xcode-select -p
rc=$?; 
if [[ $rc != 0 ]]; 
then 
    XCODE=0
    osascript -e "tell app \"System Events\" to display dialog \"Xcode commandline tools not installed, spawning installation. \n\n!! Please rerun this tool after installation completes !!\""
    rc1=$?; 
    if [[ $rc1 == 0 ]]; 
    then
	echo
	echo
	echo "******************************************************"
	echo "* Requesting installation of Xcode commandline tools *"
	echo "*                                                    *"
	echo "* Please rerun  script after Xcode install           *"
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

# Figure out which OS X this is...
OSXVER=`sw_vers -productVersion|cut -f 2 -d.`
if [ "$OSXVER" == "7" ];
then
    # 10.7 aka Lion
    HPC="gcc-4.7-bin.tar.gz"
elif [ "$OSXVER" == "8" ];
then
    # 10.8 aka Mountain Lion
    HPC="gcc-4.8-bin.tar.gz"
elif [ "$OSXVER" == "9" ];
then
    # 10.9 aka Mavericks
    HPC="gcc-4.9-bin.tar.gz"
elif [ "$OSXVER" == "10" ];
then
    # 10.10 aka Yosemite
    HPC="gcc-5.1-bin.tar.gz"
elif [ "$OSXVER" == "11" ];
then
    # 10.11 aka El Capitan
    HPC="gcc-6.3-bin.tar.gz"
elif [ "$OSXVER" == "12" ];
then
    # 10.12 aka Sierra
    HPC="gcc-6.3-bin.tar.gz"
else
    echo "Your macOS is version $OSXVER which is unfortunately not supported for install of gcc from hpc.sourceforge.net"
    HPC="NONE"
fi

if [ "$HPC" != "NONE" ];
then
    HPCURL="http://downloads.sourceforge.net/hpc/${HPC}"
    # Download HPC package
    osascript -e "tell app \"System Events\" to display dialog \"Download and install gcc from $HPC?\n\n !! Requires entry of password in the terminal !! \""
    rc1=$?; 
    if [[ $rc1 == 0 ]]; 
    then
	echo "******************************************************"
	echo "* Downloading $HPC to your Download folder "
	echo "* using the curl command: "
	echo "******************************************************"
	echo
	HERE=$PWD
	cd $HOME/Downloads
	curl -L -O $HPCURL
	echo 
	echo "Download done - requesting unpack in / with root permission"
        echo
        echo "cd / ; sudo tar xzf $HOME/Downloads/$HPC"
	cd / ; sudo tar xzf $HOME/Downloads/$HPC
	echo
	echo gcc was installed in /usr/local
	sleep 3
	cd $HERE
    else
	echo "gcc from hpc.sourceforge.net was NOT installed!"
    fi
fi

cd `dirname $0`
osascript -e "tell app \"System Events\" to display dialog \"Should I attempt to download and build openmpi? \n(Will build without fortran bindings!)\n\n!!Please keep an eye out for the sudo password prompt!! \""
rc1=$?; 
if [[ $rc1 == 0 ]]; 
then
    OPENMPI="openmpi-2.0.1"
    echo "******************************************************"
    echo "* Downloading $OPENMPI.tar.gz to your Download folder "
    echo "* using the curl command: "
    echo "******************************************************"
    echo
    HERE=$PWD
    cd $HOME/Downloads
    curl -O https://www.open-mpi.org/software/ompi/v2.0/downloads/$OPENMPI.tar.gz
    echo "Download done - requesting unpack and build"
    tar xzf $OPENMPI.tar.gz
    cd $OPENMPI
    ./configure --disable-fortran --disable-mpi-fortran 
    make
    echo "******************************************************"
    echo "* Hopefully the build is now done...                 *"
    echo "* Attepmting install via sudo                        *"
    echo "******************************************************"
    echo
    echo sudo make install
    sudo make install
    cd $HERE
    echo openmpi was installed in /usr/local
    sleep 3
else
    echo Skipping install of openmpi
fi

#!/bin/bash

echo
echo Locating newest McCode package in /Applications/...
echo
NEWESTAPP=`ls -art /Applications | grep 'McStas\|McXtrace' | grep \.app | tail -1`
echo Seems /Applications/$NEWESTAPP is where I should go...
echo
MCCODE=`echo $NEWESTAPP | cut -f2 -d/ | cut -f1 -d- | tr '[:upper:]' '[:lower:]'`
RELEASE=`echo $NEWESTAPP | cut -f2 -d/ | cut -f2 -d- | tr '[:upper:]' '[:lower:]'`
RELEASE=${RELEASE%%.app}

# Ask user if the app was dragged to /Applications
osascript -e "tell app \"System Events\" to display dialog \"Did you drag the McCode.app bundle to /Applications? \n Newest available is /Applications/$NEWESTAPP \n-i.e. $MCCODE v. $RELEASE\n (Parts of this script could easily fail if this does not look right!) \""
rc1=$?; 
if [[ $rc1 == 0 ]]; 
then
    echo "OK, proceeding!"
else
    echo "OK, exiting for you to drag the app"
    exit 1
fi

# Check if Xcode commandline tools is installed

# xcode-select --print-path to check, otherwise
xcode-select --print-path
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

# Check if development headers are in place, especially math.h...
if [ ! -f /usr/include/math.h ]; then
    echo "math.h header NOT found in /usr/include!"
    echo "Attemping to locate installable package with headers "
    HEADERS=`find /Library/Developer/CommandLineTools/Packages -name *pkg`
    NUMHEADERS=`find /Library/Developer/CommandLineTools/Packages -name *pkg | wc -l | bc`
    echo $NUMHEADERS
    echo $HEADERS - i.e. $NUMHEADERS
    if [[ "$NUMHEADERS" == "1" ]];
    then
	echo "Excellent, one header package found, attempting install!"
	osascript -e "tell app \"System Events\" to display dialog \"Development headers were not installed in /usr/include and I found a matching package \n\n $HEADERS \n\n--> Suggesting to spawn installation. \n\n Please rerun this tool after installation completes !!\""
	rc1=$?;
	if [[ $rc1 == 0 ]]; 
	then
	    echo
	    echo
	    echo "******************************************************"
	    echo "* Requesting installation of development headers!    *"
	    echo "*                                                    *"
	    echo "* Please rerun dependency script after that install  *"
	    echo "******************************************************"
	    echo
	    sleep 3
	    open $HEADERS
	    exit 0
	else
	    echo
	    echo
	    echo "!! Not requesting header package install !!"
	    echo
	    echo
	    sleep 3
            exit 1
	fi
    elif [[ "$NUMHEADERS" == "0" ]];
    then
	echo "Argh! No package found, inform user!"
	osascript -e "tell app \"System Events\" to display dialog \"Development headers were not installed in /usr/include and I did NOT FIND a matching package to install! Please ask McCode Developers what to do with your given version of macOS and Xcode tools!!!\""
	exit 1
    else
	echo "Argh! More than one package found, inform user!"
	osascript -e "tell app \"System Events\" to display dialog \"Development headers were not installed in /usr/include and I found several possible packages install! Please try to pick one yourself from the folder that will now open...!!!\""
	if [[ $rc1 == 0 ]]; 
	then
	    echo
	    echo
	    echo "******************************************************"
	    echo "* Opening folder with possible header packages!      *"
	    echo "*                                                    *"
	    echo "* Please rerun dependency script installing one...   *"
	    echo "******************************************************"
	    echo
	    sleep 3
	    open /Library/Developer/CommandLineTools/Packages
	    exit 0
	else
	    echo
	    echo
	    echo "!! Not opening package directory !!"
	    echo
	    echo
	    sleep 3
            exit 1
	fi
    fi
else
    echo Excellent, math.h header already found in /usr/include!
fi

ENVSCRIPT=`ls /Applications/$NEWESTAPP/Contents/Resources/mc*/*/environment`
if [ -f $ENVSCRIPT ]; then
    echo $ENVSCRIPT
    VERSION=`dirname $ENVSCRIPT`
    echo $VERSION
    FLAVOR=`dirname $VERSION`
    VERSION=`basename $VERSION`
    FLAVOR=`basename $FLAVOR`
    osascript -e "tell app \"System Events\" to display dialog \"Do you want to define the Unix command \n\n$FLAVOR-$VERSION-environment \n\nfor easy terminal acess to $NEWESTAPP? \n\n (Please give your passwd to the sudo command in the terminal...) \""
    rc1=$?; 
    if [[ $rc1 == 0 ]]; 
    then
	echo sudo mkdir -p /usr/local/bin && sudo ln -sf $ENVSCRIPT /usr/local/bin/$FLAVOR-$VERSION-environment 
	sudo mkdir -p /usr/local/bin && sudo ln -sf $ENVSCRIPT /usr/local/bin/$FLAVOR-$VERSION-environment
    else
	echo "Not adding /usr/local/bin/$FLAVOR-$VERSION-environment "
    fi
fi

osascript -e "tell app \"System Events\" to display dialog \"Allow embedded openmpi binaries in $NEWESTAPP to send/receive through your macOS firewall? \n\n (Please give your passwd to the sudo command in the terminal...) \""
rc1=$?; 
if [[ $rc1 == 0 ]]; 
then
    echo sudo /usr/libexec/ApplicationFirewall/socketfilterfw --add /Applications/$NEWESTAPP/Contents/Resources/$MCCODE/$RELEASE/miniconda3/bin/orted
    sudo /usr/libexec/ApplicationFirewall/socketfilterfw --add /Applications/$NEWESTAPP/Contents/Resources/$MCCODE/$RELEASE/miniconda3/bin/orted
    echo sudo /usr/libexec/ApplicationFirewall/socketfilterfw --add /Applications/$NEWESTAPP/Contents/Resources/$MCCODE/$RELEASE/miniconda3/bin/orterun
    sudo /usr/libexec/ApplicationFirewall/socketfilterfw --add /Applications/$NEWESTAPP/Contents/Resources/$MCCODE/$RELEASE/miniconda3/bin/orterun
else
    echo "Not allowing access for openmpi binaries..."
fi

PACKAGEDIR=`dirname $0`
cd $PACKAGEDIR
ENVSCRIPT=`ls -art | grep environment | grep \.command | tail -1`
if [ "x$ENVSCRIPT" != "x" ]; then
    osascript -e "tell app \"System Events\" to display dialog \"It looks like you did not move your environment script $ENVSCRIPT to /Applications? \n\n We recommend that to run McStas/McXtrace from commmandline - do you want to do this? \""
    rc1=$?; 
    if [[ $rc1 == 0 ]]; 
    then
	echo mv $ENVSCRIPT /Applications/
	mv $ENVSCRIPT /Applications/
    else
	echo "Not moving the $ENVSCRIPT to /Applications..."
    fi
fi
cd -

osascript -e "tell app \"System Events\" to display dialog \"We recommend that your user $USER takes ownership of /usr/local - do you want to do this? \n\n (Please give your passwd to the sudo command in the terminal...) \""
rc1=$?; 
if [[ $rc1 == 0 ]]; 
then
    echo sudo chown -R $USER:staff /usr/local/*
    sudo chown -R $USER:staff /usr/local/*
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


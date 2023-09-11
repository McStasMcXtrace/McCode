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

# Ask user if the app attributes should be reset?
osascript -e "tell app \"System Events\" to display dialog \"Do you want me to reset attributes on the McCode.app? \n (This should bypass macOS checks on the bundle and allow you to open the app immediately after installation) \""
rc1=$?; 
if [[ $rc1 == 0 ]]; 
then
    echo "OK, resetting attributes - may take a while to complete..."
    xattr -d -r com.apple.quarantine /Applications/$NEWESTAPP 
else
    echo "OK, not resetting attributes"
fi


# Check if Xcode commandline tools is installed

# xcode-select --print-path to check, otherwise
xcode-select --print-path
rc=$?; 
if [[ $rc != 0 ]]; 
then 
    osascript -e "tell app \"System Events\" to display dialog \"Xcode commandline tools not installed, spawning installation. \n\n!! Please give your password for sudo access in the terminal !!\n\n!! Please rerun this tool after installation completes !!\""
    rc1=$?; 
    if [[ $rc1 == 0 ]]; 
    then
	echo
	echo
	echo "*********************************************************"
	echo "* Requesting installation of Xcode commandline tools    *"
	echo "* in terminal!                                          *"
	echo "*                                                       *"
	echo "* Please give your password for sudo access if prompted *" 
	echo "*                                                       *"
	echo "* Please rerun dependency script after Xcode install    *"
	echo "*********************************************************"
	echo
	sleep 3
	sudo Xcode-select --reset
	sudo xcode-select --install
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

# homebrew Arm on mac?
if [ -d /opt/homebrew/share/gtksourceview-4/language-specs/ ];
then
    find /Applications/${NEWESTAPP} -name mccode.lang -exec ln -sf \{\} /opt/homebrew/share/gtksourceview-4/language-specs/ \;
fi
# homebrew Intel on mac?
if [ -d /usr/local/share/gtksourceview-4/language-specs/ ];
then
    find /Applications/${NEWESTAPP} -name mccode.lang -exec ln -sf \{\} /usr/local/share/gtksourceview-4/language-specs/ \;
fi


ENVSCRIPT=`find /Applications/$NEWESTAPP/Contents/Resources/ -name \*-environment`
if [ -f $ENVSCRIPT ]; then
    VERSION=$RELEASE
    FLAVOR=$MCCODE
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
    ORTED=`find /Applications/$NEWESTAPP/Contents/Resources -type f -name orted`
    ORTERUN=`find /Applications/$NEWESTAPP/Contents/Resources -type f -name orterun`
    echo sudo /usr/libexec/ApplicationFirewall/socketfilterfw --add $ORTED
    sudo /usr/libexec/ApplicationFirewall/socketfilterfw --add $ORTED
    echo sudo /usr/libexec/ApplicationFirewall/socketfilterfw --add $ORTERUN
    sudo /usr/libexec/ApplicationFirewall/socketfilterfw --add $ORTERUN
else
    echo "Not allowing access for openmpi binaries..."
fi

PACKAGEDIR=`dirname $0`
cd $PACKAGEDIR
ENVSCRIPT=`ls -art | grep ${ENVSCRIPT} | grep \.command | tail -1`
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


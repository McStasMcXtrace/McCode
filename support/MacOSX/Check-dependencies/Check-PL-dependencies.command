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

# Check if XQuartz is installed 
if [ -d "/Applications/Utilities/XQuartz.app" ]; then
    echo Xquartz is already installed!
else
    osascript -e "tell app \"System Events\" to display dialog \"Your system is missing XQuartz.app - Please download, install XQuartz, reboot your OS X\n\n !! Afterwards please rerun this tool !!\""
    if [ -d "/Applications/Utilities/X11.app" ]; then
	echo
	echo
	echo "******************************************************"
	echo "* Requesting installation of Xquartz via X11.app     *"
	echo "*                                                    *"
	echo "* Please rerun dependency script after installation  *"
	echo "******************************************************"
	echo
	sleep 3
	open -a X11.app
	exit 0
    else
	echo
	echo "******************************************************"
	echo "* Spawning browser for downloading XQuartz           *"
	echo "*                                                    *"
	echo "* Please rerun dependency script after that install *"
	echo "******************************************************"
	echo   
	sleep 3
	open https://www.xquartz.org
	exit 0
    fi
fi

# Figure out which OS X this is...
OSXVER=`sw_vers -productVersion|cut -f 2 -d.`
OSXVER_MAJOR=`sw_vers -productVersion|cut -f 1 -d.`
if [ "$OSXVER" == "6" ];
then
    # 10.6 aka Snow Leopard
    PERLVER="5.8.9"
    TKPKG="Tk-804_030_MacOSX_10_6_Perl_5_8_9.pkg.zip"
    SCIPDL="SciKarl-Intel-v0.12.pkg.zip"
elif [ "$OSXVER" == "7" ];
then
    # 10.7 aka Lion
    PERLVER="5.12"
    TKPKG="Tk-804_030_MacOSX_10_7_Perl_5_12.pkg.zip"
    SCIPDL="SciPDL-v2.4.10-Lion.pkg.zip"
elif [ "$OSXVER" == "8" ];
then
    # 10.8 aka Mountain Lion
    PERLVER="5.12"
    TKPKG="Tk-804_030_MacOSX_10_8_Perl_5_12.pkg.zip"
    SCIPDL="SciPDL-v2.4.10-Lion.pkg.zip"
elif [ "$OSXVER" == "9" ];
then
    # 10.9 aka Mavericks
    PERLVER="5.12"
    TKPKG="Tk-804_030_MacOSX_10_8_Perl_5_12.pkg.zip"
    SCIPDL="SciPDL-v2.4.10-Lion.pkg.zip"
elif [ "$OSXVER" == "10" ];
then
    # 10.10 aka Yosemite
    PERLVER="SYSTEM"
    TKPKG="Tk-804_032_MacOSX_10_10_Perl_5_18.pkg.zip"
    SCIPDL="SciPDL-v2.5-Yosemite.pkg.zip"
elif [ "$OSXVER" == "11" ];
then
    # 10.11 aka El Capitan
    PERLVER="SYSTEM"
    TKPKG="Tk-804_032_MacOSX_10_10_Perl_5_18.pkg.zip"
    SCIPDL="SciPDL-v2.5-Yosemite.pkg.zip"
elif [ "$OSXVER" == "12" ];
then
    # 10.12 aka Sierra
    PERLVER="SYSTEM"
    TKPKG="Tk-804_032_MacOSX_10_10_Perl_5_18.pkg.zip"
    SCIPDL="SciPDL-v2.5-Yosemite.pkg.zip"
elif [ "$OSXVER" == "13" ];
then
    # 10.13 aka High Sierra
    PERLVER="SYSTEM"
    TKPKG="Tk-804_032_MacOSX_10_10_Perl_5_18.pkg.zip"
    SCIPDL="SciPDL-v2.5-Yosemite.pkg.zip"
elif [ "$OSXVER" == "14" ];
then
    # 10.14 aka Mojave
    PERLVER="SYSTEM"
    TKPKG="Tk-804_032_MacOSX_10_10_Perl_5_18.pkg.zip"
    SCIPDL="SciPDL-v2.5-Yosemite.pkg.zip"
elif [ "$OSXVER" == "15" ];
then
    # 10.15 aka Catalina
    PERLVER="SYSTEM"
    TKPKG="Tk-804_032_MacOSX_10_10_Perl_5_18.pkg.zip"
    SCIPDL="SciPDL-v2.5-Yosemite.pkg.zip"
elif [ "$OSXVER" == "0" ];
then
    if [ "$OSXVER_MAJOR" == "11" ]
    then
	# 11.0 aka Big Sur
	PERLVER="SCIPDL"
	TKPKG=""
	SCIPDL="SciPDL-v2.019.dmg"
    fi
else
    osascript -e "tell app \"System Events\" to display dialog \"Your macOS is version $OSXVER is not confirmed to work with the the McCode perl tools... Would you like to attempt installation of the tools known to work with High Sierra, Mojave and Catalina?\""
    rc1=$?; 
    if [[ $rc1 == 0 ]]; 
    then    
        # Assume that what works for 10.13 and 10.14 will also work here
	PERLVER="SYSTEM"
	TKPKG="Tk-804_032_MacOSX_10_10_Perl_5_18.pkg.zip"
	SCIPDL="SciPDL-v2.5-Yosemite.pkg.zip"
    else
	echo "OK, cancelling install"
	exit 1;
    fi
fi

# Download support packages
osascript -e "tell app \"System Events\" to display dialog \"Will now request download of the packages \n$TKPKG and \n$SCIPDL\n from the McCode GitHub page\n\n !! Please locate the your download folder and install!! \""
rc1=$?; 
if [[ $rc1 == 0 ]]; 
then
    echo
    echo "******************************************************"
    echo "* Spawning browser for downloading package(s) from   *"
    echo "* the mccode GitHub:                            *"
    echo "******************************************************"
    if [ -z "$TKPKG" ] 
    then
	echo not downloading TK
    else
	TKPKG="https://github.com/McStasMcXtrace/McCode/blob/master/support/MacOSX/Perl-Tk/${TKPKG}?raw=true"
    fi
    SCIPDL="https://github.com/McStasMcXtrace/McCode/blob/master/support/MacOSX/SciPDL/${SCIPDL}?raw=true"
    # Proceed to download and install SciPDL + Tk packages

    echo $TKPKG
    echo $SCIPDL
    
    sleep 3
    if [ -z "$TKPKG" ] 
    then
	`open $TKPKG`
    fi
    `open $SCIPDL`
else
    echo "McCode support packages $TKPKG and $SCIPDL will NOT be downloaded!"
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

# If we are not using the SYSTEM perl, patch the perl scripts to use the relevant version...
if [ "$PERLVER" != "SYSTEM" ];
then
    osascript -e "tell app \"System Events\" to display dialog \"Installed support packages requires the use of Perl $PERLVER... \n Proceed to patch McCode app $NEWESTAPP for use with Perl $PERLVER?? \""
    rc1=$?; 
    if [[ $rc1 == 0 ]]; 
    then
	if [ -d "/Applications/$NEWESTAPP/Contents/Resources/mcstas" ];
	then
    	    cd /Applications/$NEWESTAPP/Contents/Resources/mcstas/*/bin
    	    sed -i.bak s+\#\!\ /usr/bin/perl5.18+\#\!/usr/bin/perl$PERLVER+ *.pl
	    sed -i.bak s+\#\!\ /usr/bin/perl5.18+\#\!/usr/bin/perl$PERLVER+ mcdoc
	elif [ -d "/Applications/$NEWESTAPP/Contents/Resources/mcxtrace" ];
	then
    	    cd /Applications/$NEWESTAPP/Contents/Resources/mcxtrace/*/bin
    	    sed -i.bak s+\#\!\ /usr/bin/perl5.18+\#\!/usr/bin/perl$PERLVER+ *.pl
	    sed -i.bak s+\#\!\ /usr/bin/perl5.18+\#\!/usr/bin/perl$PERLVER+ mxdoc
	else
    	    echo "Your app $NEWESTAPP seems to be neither McStas nor McXtrace... Aborting!"
    	    exit 1
	fi
    else
	echo "Did not patch McCode package for use with Perl version $PERLVER!"
	exit 1
    fi
else
    echo Excellent, your default Perl is the right version, proceeding!
fi

cd /Applications
osascript -e "tell app \"System Events\" to display dialog \"Should I patch $NEWESTAPP to run the Perl GUI? (default is otherwise Python)\""
rc1=$?; 
if [[ $rc1 == 0 ]]; 
then
    if [ -d "$NEWESTAPP/Contents/Resources/" ];
    then
    	cd $NEWESTAPP/Contents/Resources/
	cp launcher-pl.sh launcher.sh
    else
    	echo "Your app $NEWESTAPP seems corrupted, or with wrong layout... Aborting!"
    	exit 1
    fi
else
    echo "Did not patch McCode package to run the Perl GUI"
fi


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

# Offer to download instantplayer for viewing VRML files
osascript -e "tell app \"System Events\" to display dialog \"Do you want to download InstantPlayer for viewing VRML/x3d files? \n (Will spawn browser for you...) \""
rc1=$?; 
if [[ $rc1 == 0 ]]; 
then
    open http://www.instantreality.org/downloads
else
    echo Not spawning download of InstantPlayer
fi


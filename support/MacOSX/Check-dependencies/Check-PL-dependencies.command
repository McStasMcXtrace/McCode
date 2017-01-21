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
else
    echo "Your macOS is version $OSXVER which is unfortunately not supported for the McCode perl tools..."
    exit 1
fi

# Download support packages
osascript -e "tell app \"System Events\" to display dialog \"Will now request download of the packages \n$TKPKG and \n$SCIPDL\n from the McCode GitHub page\n\n !! Please locate the your download folder and install!! \""
rc1=$?; 
if [[ $rc1 == 0 ]]; 
then
    TKPKG="https://github.com/McStasMcXtrace/McCode/blob/master/support/MacOSX/Perl-Tk/${TKPKG}?raw=true"
    SCIPDL="https://github.com/McStasMcXtrace/McCode/blob/master/support/MacOSX/SciPDL/${SCIPDL}?raw=true"
    # Proceed to download and install SciPDL + Tk packages
    echo
    echo "******************************************************"
    echo "* Spawning browser for downloading these packages    *"
    echo "* from the mccode GitHub:                            *"
    echo "******************************************************"
    echo $TKPKG
    echo $SCIPDL
    
    sleep 3
    `open $TKPKG`
    `open $SCIPDL`
else
    echo "McCode support packages $TKPKG and $SCIPDL will NOT be downloaded!"
fi

cd `dirname $0`
echo
echo Locating newest McCode package in $PWD/...
echo
NEWESTAPP=`ls -rt | grep Mc | grep \.app`
echo Seems $NEWESTAPP is where I should go...
echo
# If we are not using the SYSTEM perl, patch the perl scripts to use the relevant version...
if [ "$PERLVER" != "SYSTEM" ];
then
    osascript -e "tell app \"System Events\" to display dialog \"Installed support packages requires the use of Perl $PERLVER... \n Proceed to patch McCode app $NEWESTAPP for use with Perl $PERLVER?? \""
    rc1=$?; 
    if [[ $rc1 == 0 ]]; 
    then
	if [ -d "$NEWESTAPP/Contents/Resources/mcstas" ];
	then
    	    cd $NEWESTAPP/Contents/Resources/mcstas/*/bin
    	    sed -i.bak s+\#\!\ /usr/bin/perl5.18+\#\!/usr/bin/perl$PERLVER+ *.pl
	    sed -i.bak s+\#\!\ /usr/bin/perl5.18+\#\!/usr/bin/perl$PERLVER+ mcdoc
	elif [ -d "$NEWESTAPP/Contents/Resources/mcxtrace" ];
	then
    	    cd $NEWESTAPP/Contents/Resources/mcxtrace/*/bin
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

cd `dirname $0`
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

# Offer to download instantplayer for viewing VRML files
osascript -e "tell app \"System Events\" to display dialog \"Do you want to download InstantPlayer for viewing VRML/x3d files? \n (Will spawn browser for you...) \""
rc1=$?; 
if [[ $rc1 == 0 ]]; 
then
    open http://www.instantreality.org/downloads
else
    echo Not spawning download of InstantPlayer
fi


# TODO:
# Offer to get gcc from hpc.sourceforge.net
# Offer to spawn (c-only) build of openmpi-2.0
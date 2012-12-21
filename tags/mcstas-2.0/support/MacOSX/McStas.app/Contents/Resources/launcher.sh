#!/bin/bash
#
# Script for Mac OS X to launch a command-line program with arguments.
# by Martin Fuhrer (mfuhrer@alumni.ucalgary.ca)
# Permission to modify and distribute granted.
#

# program name
PROGRAM=mcgui

# program arguments
ARGUMENTS=""

# working directory
DIRECTORY="~"

# paths
PATH=/usr/local/bin:/sw/bin:/sw/sbin:/opt/local/bin:/opt/local/sbin:/usr/X11R6/bin:/bin:/usr/bin:/sbin:/usr/sbin:/usr/bin/X11:/usr/local/bin/X11:$PATH
DYLD_LIBRARY_PATH=/sw/lib:/opt/local/lib:$DYLD_LIBRARY_PATH
LD_LIBRARY_PATH=/sw/lib:/opt/local/lib:$LD_LIBRARY_PATH

# environment variables
MCSTAS=/usr/local/lib/mcstas
PATH=/usr/local/bin:$PATH


# require X11
USEX11=1

# X11 environment (eg. X11, XDarwin, OroborOSX)
X11=X11

# try to find absolute path of X11 environment 
if [ -d /Applications/Utilities/"$X11".app ] 
then
  x11_app=/Applications/Utilities/"$X11".app
elif [ -d /Applications/"$X11".app ]
then
  x11_app=/Applications/"$X11".app
fi

DISPLAYDIR=/tmp/.X11-unix

manualX11Launch=0

# read preferences
PREFS=$HOME"/Library/Preferences/XDroplets.settings"
if [ -e $PREFS ]
then
  X11=`head -n 1 $PREFS`
fi

# check program availability
result=`which "$PROGRAM"`
if [ `echo $?` -ne 0 ]
then
  echo "'$PROGRAM' : program not found in path"
  exit 1 # program does not exist in path
fi
  
# launch X11 manually if necessary
if [ $USEX11 -eq 1 ]
then
  # determine the user (even if user is masquerading as root)
  ROOTUSERFILE="/tmp/xdropletsRootUser.txt"  # generated externally
  if [ -e $ROOTUSERFILE ]
  then
    user=`cat $ROOTUSERFILE`
    rm $ROOTUSERFILE
  else
    user=`whoami`
  fi
  x11Check=`ps aux -U "$user" | grep -v grep | grep "$X11".app`
  displayListing=`find "$DISPLAYDIR" -mindepth 1 -user "$user"`
  # Will return 5 if Leopard:
  osVersion=0
  osVersion+=`sw_vers | grep ProductVersion | cut -f 2 -d.`

  if [ "$osVersion" -lt 5 ]
  then
  
  if [ -z "$x11Check" ]  # check if x11 environment is running
  then
  	manualX11Launch=1;
    /usr/bin/open -a "$X11"
    if [ `echo $?` -ne 0 ]
    then
      echo "$X11.app : X11 environment could not be launched"
      exit 2 # unable to start X11 environment
    else
      # wait for window manager to start up
      while [ -z "$displayListing" ] && [ $SECONDS -le 30 ]
      do
      	sleep 1
        displayListing=`find "$DISPLAYDIR" -mindepth 1 -user "$user"`
      done
      
      if [ $SECONDS -ge 31 ]
      then
        echo "X11 window manager could not be started"
        exit 3 # unable to locate window manager
      fi
    fi
  fi

  # set DISPLAY variable for the current user
  displayNumber=${displayListing/*X/:}
  DISPLAY=$displayNumber.0    
  export DISPLAY
  fi
fi

# check if the working directory exists and change directory
DIRECTORY=${DIRECTORY/~/"$HOME"}  # replace ~ with home path
if ! [ -d "$DIRECTORY" ]
then
  DIRECTORY="$HOME"
fi
cd $DIRECTORY

# this "hack" gives the OroborOSX window manager extra time to start up
if [ $manualX11Launch = 1 ] && [ "$X11" = "OroborOSX" ]
then
	sleep 10
fi

# launch program with arguments and file paths
if [ -z "$ARGUMENTS" ]
then
  "$PROGRAM" "$@" > /dev/null 2>&1 &
else 
  "$PROGRAM" $ARGUMENTS "$@" > /dev/null 2>&1 &
fi

if [ "$osVersion" -lt 5 ]
then
  # bring X11 application to front
  if [ $USEX11 -eq 1 ]
  then
    /usr/bin/open -a "$X11"
  fi
fi
#!/bin/sh
#
# Unix migration script for McXtrace-pre 1.1 for those who want to keep that on their machine
#
#

MCXTRACE_WHERE=`which mcxtrace`
MCXTRACE_VERSION=`mcxtrace -v|head -1|cut -f3 -d\ `
MCXTRACE_MAJOR=`echo $MCXTRACE_VERSION | cut -f1 -d.`

# This check disabled for now...
# if [ ! "${MCXTRACE_MAJOR}" = 1 ]; then
#    echo "Sorry, this script is made for McXtrace versions 1.x only!";
#    exit 1;
# fi

MCXTRACE_BINDIR=`dirname $MCXTRACE_WHERE`
MCXTRACE_LIBDIR=`dirname $MCXTRACE_BINDIR`/lib/mcxtrace
MCXTRACE_NEWBINDIR=$MCXTRACE_BINDIR/McXtrace-$MCXTRACE_VERSION
MCXTRACE_NEWLIBDIR=$MCXTRACE_LIBDIR-$MCXTRACE_VERSION
MOVE_SCRIPT=/tmp/mcxtrace-$MCXTRACE_VERSION-move
ENV_SCRIPT=/tmp/mcxtrace-$MCXTRACE_VERSION-environment

# Write migration info to the screen:
cat <<EOF
Your existing mcxtrace executable is this one: $MCXTRACE_WHERE
 - version is $MCXTRACE_VERSION
 - located in $MCXTRACE_BINDIR
 - with component library in $MCXTRACE_LIBDIR

The suggested new location for the $MCXTRACE_VERSION binaries is:
  $MCXTRACE_BINDIR/McXtrace-$MCXTRACE_VERSION

With the component library in:
  $MCXTRACE_NEWLIBDIR

To move your installation, please execute the script $MOVE_SCRIPT
EOF

# Write the move script:
cat <<EOF > $MOVE_SCRIPT
#!/bin/sh
# Script for moving your McXtrace $MCXTRACE_VERSION installation out of the way before installation of 2.0

# Create directory for the binaries
sudo mkdir -p $MCXTRACE_NEWBINDIR
# Move component library to $MCXTRACE_NEWLIBDIR
sudo cp -rp $MCXTRACE_LIBDIR $MCXTRACE_NEWLIBDIR
# Move the binaries to $MCXTRACE_NEWBINDIR
EOF
for bincomp in \
    mcdaemon mcdisplay mcdoc mcformat mcformatgui mcgui \
    mcplot mcresplot mcrun mcxtrace mcxtrace2vitess
do
	  echo "sudo cp $MCXTRACE_BINDIR/$bincomp $MCXTRACE_NEWBINDIR/" >> $MOVE_SCRIPT;
done

cat <<EOF >> $MOVE_SCRIPT
# Finally, copy environment script to /usr/local/bin...
sudo cp $ENV_SCRIPT /usr/local/bin
EOF

# Write environment script info to screen
echo "";
echo "Afterwards, the script $ENV_SCRIPT can be used to run your";
echo "relocated McXtrace $MCXTRACE_VERSION.\
 (Migration script places a copy in /usr/local/bin)";
echo "";

# Write environment script info to Enviroment script
cat <<EOF > $ENV_SCRIPT
#!/bin/sh
export MCXTRACE=$MCXTRACE_NEWLIBDIR/mcxtrace
export MCSTAS=$MCXTRACE_NEWLIBDIR/mcxtrace
export PATH=$MCXTRACE_NEWBINDIR:\$PATH
echo
echo The new shell started here is now set up for running this version of mcxtrace:
echo
mcxtrace -v
echo
echo To end using this version of mcxtrace, exit this shell.
echo
export PS1='McXtrace $MCXTRACE_VERSION env \\w\\$ > '
/bin/sh
EOF

# Make the scripts executable:
chmod a+x $ENV_SCRIPT
chmod a+x $MOVE_SCRIPT

# If this is Debian and mcxtrace is installed,
# recommend to uninstall before installing 1.1
DPKG="$(command -v dpkg)";
if [ -x "${DPKG}" ]; then
	  echo "Debian machine - checking if mcxtrace was previously installed";
	  MCXTRACE_FROM_DEB=`dpkg -l \*mcxtrace\* |grep ^ii`
	  if [ "$?" = "0" ]; then
	      echo "Found these McXtrace-related packages on your system:";
	      echo "";
	      dpkg -l \*mcxtrace\* |grep ^ii ;
	      echo "";
	      echo "Our recommendation is that you uninstall the old 1.x packages"
        echo "  once you have run $MOVE_SCRIPT";
	  fi
fi

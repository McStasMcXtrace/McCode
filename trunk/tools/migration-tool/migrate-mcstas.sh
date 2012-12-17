#!/bin/sh
#
# Unix migration script for McStas-pre 2.0 for those who want to keep that on their machine
#
#

MCSTAS_WHERE=`which mcstas`
MCSTAS_VERSION=`mcstas -v|head -1|cut -f3 -d\ `
MCSTAS_MAJOR=`echo $MCSTAS_VERSION | cut -f1 -d.`
if [ ! "${MCSTAS_MAJOR}" = 1 ]; then
    echo "Sorry, this script is made for McStas versions 1.x only!";
    exit 1;
fi

MCSTAS_BINDIR=`dirname $MCSTAS_WHERE`
MCSTAS_LIBDIR=`dirname $MCSTAS_BINDIR`/lib/mcstas
MCSTAS_NEWBINDIR=$MCSTAS_BINDIR/McStas-$MCSTAS_VERSION
MCSTAS_NEWLIBDIR=$MCSTAS_LIBDIR-$MCSTAS_VERSION
MOVE_SCRIPT=/tmp/mcstas-$MCSTAS_VERSION-move
ENV_SCRIPT=/tmp/mcstas-$MCSTAS_VERSION-environment


BINS="mcconvert mcdaemon mcdisplay mcdoc mcformat mcformatgui mcgui mcplot \
      mcresplot mcrun mcstas mcstas2vitess"


# Write migration info to the screen:
cat <<EOF
Your existing mcstas executable is this one: $MCSTAS_WHERE
 - version is $MCSTAS_VERSION
 - located in $MCSTAS_BINDIR
 - with component library in $MCSTAS_LIBDIR

The suggested new location for the $MCSTAS_VERSION binaries is:
  $MCSTAS_BINDIR/McStas-$MCSTAS_VERSION

With the component library in:
  $MCSTAS_NEWLIBDIR

To move your installation, please execute the script $MOVE_SCRIPT
EOF

# Write the move script:
cat <<EOF > $MOVE_SCRIPT
#!/bin/sh
# Script for moving your McStas $MCSTAS_VERSION installation out of the way before installation of 2.0

# Create directory for the binaries
sudo mkdir -p $MCSTAS_NEWBINDIR
# Move component library to $MCSTAS_NEWLIBDIR
sudo cp -rp $MCSTAS_LIBDIR $MCSTAS_NEWLIBDIR
# Move the binaries to $MCSTAS_NEWBINDIR
EOF
for bincomp in ${BINS}; do
	  echo "sudo cp $MCSTAS_BINDIR/$bincomp $MCSTAS_NEWBINDIR/" >> $MOVE_SCRIPT;
done

cat <<EOF >> $MOVE_SCRIPT
# Finally, copy environment script to /usr/local/bin...
sudo cp $ENV_SCRIPT /usr/local/bin
EOF

# Write environment script info to screen
echo "";
echo "Afterwards, the script $ENV_SCRIPT can be used to run your";
echo "relocated McStas $MCSTAS_VERSION.\
 (Migration script places a copy in /usr/local/bin)";
echo "";

# Write environment script info to Enviroment script
cat <<EOF > $ENV_SCRIPT
#!/bin/sh
export MCSTAS=$MCSTAS_NEWLIBDIR
export PATH=$MCSTAS_NEWBINDIR:\$PATH
echo
echo The new shell started here is now set up for running this version of mcstas:
echo
mcstas -v
echo
echo To end using this version of mcstas, exit this shell.
echo
export PS1='McStas $MCSTAS_VERSION env \\w\\$ > '
/bin/sh
EOF

# Make the scripts executable:
chmod a+x $ENV_SCRIPT
chmod a+x $MOVE_SCRIPT

# If this is Debian and mcstas is installed,
# recommend to uninstall before installing 2.0
DPKG="$(command -v dpkg)";
if [ -x "${DPKG}" ]; then
	  echo "Debian machine - checking if mcstas was previously installed";
	  MCSTAS_FROM_DEB=`dpkg -l \*mcstas\* |grep ^ii`
	  if [ "$?" = "0" ]; then
	      echo "Found these McStas-related packages on your system:";
	      echo "";
	      dpkg -l \*mcstas\* |grep ^ii ;
	      echo "";
	      echo "Our recommendation is that you uninstall the old 1.x packages"
        echo "  once you have run $MOVE_SCRIPT";
	  fi
fi

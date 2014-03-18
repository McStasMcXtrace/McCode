#!/bin/sh
#
# Unix migration script for McStas 2.0 for those who want to keep that on
# their machine
#

MCSTAS_WHERE=`which mcstas`
MCSTAS_VERSION=`mcstas -v|head -1|cut -f3 -d\ `
if [ ! "${MCSTAS_VERSION}" = 2.0 ]; then
    echo "Sorry, this script is made for McStas version 2.0 only!";
    exit 1;
fi

MCSTAS_BINDIR=`dirname $MCSTAS_WHERE`
MCSTAS_LIBDIR=`dirname $MCSTAS_BINDIR`/lib/mcstas-2.0
MCSTAS_MANDIR=`dirname $MCSTAS_BINDIR`/man/man1
MCSTAS_PREFIX=`dirname $MCSTAS_BINDIR`
MCSTAS_NEWLIBDIR=$MCSTAS_PREFIX/mcstas/$MCSTAS_VERSION
MCSTAS_NEWBINDIR=$MCSTAS_NEWLIBDIR/bin
MCSTAS_NEWMANDIR=$MCSTAS_NEWLIBDIR/man
MOVE_SCRIPT=/tmp/mcstas-$MCSTAS_VERSION-move
ENV_SCRIPT=/tmp/mcstas-$MCSTAS_VERSION-environment


BINS="mcdaemon mcdisplay mcdoc mcformat mcformatgui mcgui mcplot \
      mcresplot mcrun mcstas mcstas2vitess mccode-select-bundle-mcstas-2.0 \
      mccode-select-mcstas-2.0 mcconfig mcconfig-2.0 mcconvert-2.0 mcdaemon-2.0 \
      mcdisplay-2.0 mcdoc-2.0 mcformat-2.0 mcformatgui-2.0 mcgui-2.0 \
      mcplot-2.0 mcplot-2.0-matplotlib  mcplot-2.0-py mcresplot-2.0 \
      mcrun-2.0 mcrun-2.0-py mcstas-2.0 mcstas2vitess-2.0 mcdisplay-2.0-py"

# Check if system is Debian-like and has dpkg (e.g. Ubuntu)
HAS_DPKG=false;
# DPKG="$(command -v dpkg)";
#if [ $? -eq 0 ] && [ -x "${DPKG}" ]; then
#    HAS_DPKG=true;
# fi

# Write migration info to the screen:
cat <<EOF
Your existing mcstas executable is this one: $MCSTAS_WHERE
 - version is $MCSTAS_VERSION
 - located in $MCSTAS_BINDIR
 - with component library in $MCSTAS_LIBDIR

The suggested new location for the $MCSTAS_VERSION binaries is:
  $MCSTAS_NEWBINDIR

With the component library in:
  $MCSTAS_NEWLIBDIR

1) To move your installation, please execute the script $MOVE_SCRIPT
EOF


# Write the move script:
cat <<EOF > $MOVE_SCRIPT
#!/bin/sh
# Script for moving your McStas $MCSTAS_VERSION installation out of the way
# before installation of 2.0

# Create top-level mcstas dir if it was not there already
sudo mkdir -p $MCSTAS_PREFIX/mcstas

# Move component library to $MCSTAS_NEWLIBDIR
sudo cp -rp $MCSTAS_LIBDIR $MCSTAS_NEWLIBDIR

# Create directory for the binaries and man-pages
sudo mkdir -p $MCSTAS_NEWBINDIR $MCSTAS_NEWMANDIR

# Move the binaries to $MCSTAS_NEWBINDIR
EOF


# Write code to copy binaries and man-pages
for bincomp in ${BINS}; do
    echo "# ${bincomp}" >> $MOVE_SCRIPT;
    BIN=$MCSTAS_BINDIR/$bincomp
    if [ -f ${BIN} ]; then
        echo "sudo cp ${BIN} ${MCSTAS_NEWBINDIR}/" >> $MOVE_SCRIPT;
    fi
    MAN=$MCSTAS_MANDIR/$bincomp.1
    if [ -f ${MAN} ]; then
        echo "sudo cp ${MAN} ${MCSTAS_NEWMANDIR}/" >> $MOVE_SCRIPT;
    fi
done


# Add some code to remove McStas binaries and man-pages if needed
if ! ${HAS_DPKG}; then
    # Build list of files to remove
    FILES="$MCSTAS_LIBDIR"
    for bincomp in ${BINS}; do
        BIN=$MCSTAS_BINDIR/$bincomp;
        if [ -f $BIN ]; then
            FILES="$FILES $BIN"
        fi
        MAN=$MCSTAS_MANDIR/$bincomp.1;
        if [ -f $MAN ]; then
            FILES="$FILES $MAN"
        fi
    done

    cat <<EOF >> $MOVE_SCRIPT

echo ""
echo "!! You're not running on a debian-like system and thus needs to remove"
echo "!! the McStas files manually (and not through the package manager)."
echo "!! (All McStas binaries AND man-pages must be moved from standard"
echo "!!  locations before installing a 2.x version)"
echo ""

FILES="$FILES"

echo "These are the files:"
ls \$FILES
echo ""

printf "?? Would you like me to delete these files for you? (y/n): "
read answer
if [ "x\$answer" = "xy" ]; then
    sudo rm -vr \$FILES
fi
EOF
fi


# Write code to copy environment-setup script
cat <<EOF >> $MOVE_SCRIPT

# Finally, copy environment script to /usr/local/bin...
sudo cp $ENV_SCRIPT /usr/local/bin
EOF


# Write environment script info to screen
echo "";
echo "2) Afterwards, the script $ENV_SCRIPT can be used to run your";
echo "relocated McStas $MCSTAS_VERSION.\
 (Migration script places a copy in /usr/local/bin)";
echo "";

# Write environment script info to Enviroment script
cat <<EOF > $ENV_SCRIPT
#!/bin/sh
export MCSTAS=$MCSTAS_NEWLIBDIR/
export PATH=$MCSTAS_NEWBINDIR:\$PATH
echo
echo The new shell started here is now set up for running this version of mcstas:
echo
mcstas -v
echo
echo To end using this version of mcstas, exit this shell.
echo
export PS1='McStas $MCSTAS_VERSION env \\w\\$ > '
if [ -x "\${SHELL}" ]; then
  \${SHELL}
else
  /bin/sh
fi
EOF


# Make the scripts executable:
chmod a+x $ENV_SCRIPT
chmod a+x $MOVE_SCRIPT


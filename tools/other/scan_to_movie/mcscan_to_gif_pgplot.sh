#!/bin/bash
#
# Shell script to generate a rotating gif from the scan of monitor 
# with output in McCode output format
#

if [ "x$1" = "x" ]; then
    # No arguments - show help
    echo Please provide two arguments to define:
    echo   1\) a scan folder location
    echo   2\) a monitor name
    echo e.g : $0 /some/directory/my_scan/ mymonitor.dat
    echo
    echo \(The second input may include wildcards such as \* or \?\)
    exit 1;
fi

STARTDIR=`pwd`
# Check if the first argument is a directory
if [ -d $1 ]; then
    echo OK $1 is a directory, proceeding
    cd $1
    echo Looking for subdirs
    SCANLENGTH=$(find . -type d -maxdepth 1 | cut -f2 -d/ | grep -v \\. | wc -l)
    echo Found scan of length $SCANLENGTH
    if [ -f 0/$2 ]; then
	echo Found 0th instance of $2, proceeding
    else
	echo No scan input present in location $1/0/$2
	exit 1;
    fi

    if [ "x$3" = "x" ]; then
    # No arguments - show help
	echo No extra option given
	EXTRA=""
    else
	EXTRA=$3
    fi
    echo "#!/bin/bash" > runme.sh
    echo "convert -dispose previous \\" >> runme.sh 
    for i in `seq 1 $SCANLENGTH`;
    do
	SUBDIR=`echo "$(($i-1))"`
	if [ -f $SUBDIR/$2 ]; then
	    mcplot.pl -psc $EXTRA ./$SUBDIR/$2
	    convert -rotate 90 $SUBDIR/$2.ps $SUBDIR/$2.png
	fi
	echo "$SUBDIR/$2.png \\"
	echo "$SUBDIR/$2.png \\" >> runme.sh
    done
    echo " $STARTDIR/Movie_$2.gif" >> runme.sh
    echo " find . -name \*.ps -exec rm \{\} \; " >> runme.sh
    echo " find . -name \*.png -exec rm \{\} \; " >> runme.sh
    chmod a+x runme.sh
    echo Running movie creation / cleanup commands...
    ./runme.sh
    echo Done!
else
    exit 1;
fi

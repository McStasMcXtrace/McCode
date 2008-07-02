#!/bin/sh
#
#    This file is part of the McStas neutron ray-trace simulation package
#    Copyright (C) 1997-2004, All rights reserved
#    Risoe National Laborartory, Roskilde, Denmark
#    Institut Laue Langevin, Grenoble, France
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; version 2 of the License.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#    compiletest.sh - Developer tool / simple shell script for compilation 
#    check of all example instruments. To be used/checked before release of
#    new 'stable' releases of McStas
#    


if [ $MCSTAS ]
then
  echo "McStas environment var set"
else
  MCSTAS="/usr/local/lib/mcstas"
fi

EXAMPLES="$MCSTAS/examples"
export EXMPLES
EXECDIR=`mktemp -d -t McRun`
export EXECDIR

echo "Example folder used is $EXAMPLES - compiling in $EXECDIR"

for file in `find $MCSTAS -name \*.comp | grep -v obsolete | grep -v Adapt | grep -v Res_mon | grep -v PreMonitor_nD | grep -v Monitor_Optimizer | grep -v Pol `;
do
    comp=`basename $file .comp`
    instr=`echo $EXECDIR/cTest_$comp.instr`
    export comp
    export instr
    echo Creating $instr
    # Extract parameter names and set to 0...
    # DEFINITION PARMS:
    echo "DEFINE INSTRUMENT Test_$comp()" > $instr
    echo "TRACE" >> $instr
    echo "COMPONENT Test = $comp(" >> $instr
    first=1
    export first
    for parm in `grep SETTING -A 100 $file | grep -B 100 OUTPUT | sed -e 's/\"/\\\\"/g' | xargs echo | cut -f1 -d \) | cut -f2 -d \( | sed -e 's/int //g' | sed -e 's/double //g' | sed -e 's/string //g' | sed -e 's/char //g' | sed -e 's/\*//g' | sed -e 's/ =/=/g' | sed -e 's/= /=/g' | sed -e 's/ //g' | sed -e 's/,/ /g'`;
    do
	if [ "$first" != "1"  ]; then
	    echo ", " >> $instr
	else
	  first=0
	  export first 
	fi
	eq=`echo $parm | grep -c \=`
	if [ "$eq" == "0"  ]; then
	  echo `echo $parm` = 1 >> $instr
	else
	  echo `echo $parm` >> $instr  
	fi   
    done
    for parm in `grep DEFINITION -A 100 $file | grep -B 100 SETTING | sed -e 's/\"/\\\\"/g' | xargs echo | cut -f1 -d \) | cut -f2 -d \( | sed -e 's/int //g' | sed -e 's/double //g' | sed -e 's/string //g' | sed -e 's/char //g' | sed -e 's/\*//g' | sed -e 's/ =/=/g' | sed -e 's/= /=/g' | sed -e 's/ //g' | sed -e 's/,/ /g'`;
    do
	if [ "$first" != "1"  ]; then
	    echo ", " >> $instr
	else
	  first=0
	  export first 
	fi
	eq=`echo $parm | grep -c \=`
	if [ "$eq" == "0"  ]; then
	  echo `echo $parm ` = 1 >> $instr
	else
	  echo `echo $parm` >> $instr  
	fi    
    done
    echo ") AT (0,0,0) ABSOLUTE" >> $instr
    echo END >> $instr
done    

if [ -e $EXAMPLES ]
then
  echo "Copying example instruments to $EXECDIR"
  cp $EXAMPLES/*.instr $EXECDIR
  cd $EXECDIR
  NUMINSTR=`ls *.instr | wc -l`
  echo Test-compiling $NUMINSTR instrument files...
  for instr in `ls *.instr`
  do
    mcrun -n0 $instr > /dev/null
    echo $instr done
  done
else
  echo "Sorry, $EXAMPLES did not exist"
  return 1
fi

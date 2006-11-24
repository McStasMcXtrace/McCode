#!/bin/sh
#
#    This file is part of the McStas neutron ray-trace simulation package
#    Copyright (C) 1997-2004, All rights reserved
#    Risoe National Laborartory, Roskilde, Denmark
#    Institut Laue Langevin, Grenoble, France
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
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
EXECDIR=`mktemp -d`

echo "Example folder used is $EXAMPLES"

if [ -e $EXAMPLES ]
then
  echo "Copying example instruments to $EXECDIR"
  cp $EXAMPLES/*.instr $EXECDIR
  cd $EXECDIR
  #for instr in `ls *.instr`
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


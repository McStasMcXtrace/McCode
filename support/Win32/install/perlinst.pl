#    This file is part of the McStas neutron ray-trace simulation package
#    Copyright (C) 1997-2004, All rights reserved
#    Risoe National Laborartory, Roskilde, Denmark
#    Institut Laue Langevin, Grenoble, France
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; version 3 of the License.
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
# perlinst.pl:
#
#    Special Win32 Perl script for installing McStas modules
#    in specific perl folders, e.g. for installing McStas.pm
#    in the folder where Bash.pm is already:
#
#    McStas.pm -> PERLLIB\Tk\CodeText\Bash.pm
# 
#     ARGV[1]             ----- ARGV[0] -----

use File::Basename;
use Win32;

print $_;

if (@ARGV>1) {
    my $tofind = $ARGV[0];
    my $tocopy = $ARGV[1];
    my $found = 0;
    my $endfile = basename($tocopy);
    if (-e $tocopy) {
	my $Location;
	for ($j=0; $j<@INC; $j++) {
	    my $filename = "$INC[$j]/$tofind";
	    if (-e $filename) {
		$Location = dirname($filename);
		# This is for Win32 only, we should probably
		print STDOUT "Installing\n\n$tocopy\n\nin perl folder\n\n$Location\n";
		if (-e "$Location/$endfile") {
		    print STDOUT "Unlinking existing $Location/$endfile\n";
		    unlink("$Location/$endfile");
                }
	        Win32::CopyFile($tocopy, "$Location/$endfile", 1);
		$found = 1;
		exit;
	    }
	}
	if ($found == 0) {
	    die "Sorry: Never found directory in INC containing $tofind...\n";
	}
    } else {
	die "Given filename $tocopy does not exist!\n";
    }
} else { 
    die "Please give me:\n a) an input module to search the Perl lib for, e.g. \n     Tk\\CodeText\\Bash.pm\n b) a module to copy to that location, e.g.\n     support\\Tk-CodeText-0.3.4\\CodeText\\McStas.pm\n";
}

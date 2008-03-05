#!/usr/bin/perl
#
# Simple method to provide 'Press any key to exit' when ending our
# Win32 commands
#

use Config;

if ($Config{'osname'} eq 'MSWin32') {
    system(join(' ',@ARGV));
    system('pause');
    exit 1;
} else {
    die "This util is only meant for the Win32 platform\n";
}


#!/usr/bin/perl -w
# -*- perl -*-

# Test case from https://rt.cpan.org/Ticket/Display.html?id=42043
# Fails if XFT=1 is activated.
# Not in normal test suite, because result depends on fonts installed on system.

use strict;

use Getopt::Long;

use Tk;
use Tk::Config;
use Tk::Font;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip no Test::More module\n";
	exit;
    }
}

my $Xft = $Tk::Config::xlib =~ /-lXft\b/;
if (!$Xft) {
    plan skip_all => 'Hardcoded font values work only for XFT=1';
    exit 0;
}

plan tests => 5;

my $do_display;
GetOptions("display" => \$do_display)
    or die "usage: $0 [-display]";

my $mw = MainWindow->new;

{
    # The empty array does not work at all for XFT=0
    my $label = $mw->Label(-text=>"small font, no bold",-font=>[])->pack;
    my %fa = $label->cget(-font)->actual;
    is_deeply(\%fa, { -weight => 'normal',
		      -underline => 0,
		      -family => 'Bitstream Vera Sans',
		      -slant => 'roman',
		      -size => -14,
		      -overstrike => 0,
		    }, 'Unchanged font size, normal weight');
}

{
    my $label = $mw->Label(-text=>"small font, bold",-font=>[-weight=>'bold'])->pack;
    my %fa = $label->cget(-font)->actual;
    is_deeply(\%fa, { -weight => 'bold',
		      -underline => 0,
		      -family => 'Bitstream Vera Sans',
		      -slant => 'roman',
		      -size => -14,
		      -overstrike => 0,
		    }, 'Unchanged font size, bold');
}

{
    my $label = $mw->Label(-text=>"32-point, bold",-font=>[-size=>'32', -weight=>'bold'])->pack;
    my %fa = $label->cget(-font)->actual;
    is_deeply(\%fa, { -weight => 'bold',
		      -underline => 0,
		      -family => 'Bitstream Vera Sans',
		      -slant => 'roman',
		      -size => -38,
		      -overstrike => 0,
		    }, '32 point size, bold');
}

{
    my $label = $mw->Label(-text=>"32-point, bold, string spec",-font=>'{sans serif} 32 bold')->pack;
    my %fa = $label->cget(-font)->actual;
    is_deeply(\%fa, { -weight => 'bold',
		      -underline => 0,
		      -family => 'Bitstream Vera Sans',
		      -slant => 'roman',
		      -size => -38,
		      -overstrike => 0,
		    }, '32 point size, bold');
}

{
    my $label = $mw->Label(-text=>"32-point, no bold",-font=>[-size=>'32'])->pack;
    my %fa = $label->cget(-font)->actual;
    is_deeply(\%fa, { -weight => 'normal',
		      -underline => 0,
		      -family => 'Bitstream Vera Sans',
		      -slant => 'roman',
		      -size => -38,
		      -overstrike => 0,
		    }, '32 point size, normal weight');
}

if ($do_display) {
    my $button = $mw->Button(-text => "Quit", 
			     -command => sub { exit })
	->pack();
    MainLoop;
}

__END__

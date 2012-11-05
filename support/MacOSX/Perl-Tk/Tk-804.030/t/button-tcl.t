#!/usr/bin/perl -w
# -*- perl -*-

# This file is a Tcl script to test labels, buttons, checkbuttons, and
# radiobuttons in Tk (i.e., all the widgets defined in tkButton.c).  It is
# organized in the standard fashion for Tcl tests.
#
# Copyright (c) 1994 The Regents of the University of California.
# Copyright (c) 1994-1996 Sun Microsystems, Inc.
# Copyright (c) 1998-1999 by Scriptics Corporation.
# All rights reserved.
#
# RCS: @(#) $Id: button.test,v 1.20 2004/12/07 21:22:19 dgp Exp $

#
# Translated by Slaven Rezic (2007-07, from CVS version 1.20)
#

use strict;

use Tk;
use Getopt::Long;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

plan tests => 288;

my $mw = MainWindow->new;
$mw->geometry("+10+10");

# proc bogusTrace args {
#     error "trace aborted"
# }
my $value;
my $value2;

# Create entries in the option database to be sure that geometry options
# like border width have predictable values.

$mw->option('add', '*Button.borderWidth', 2);
$mw->option('add', '*Button.highlightThickness', 2);
$mw->option('add', '*Button.font', '{Helvetica -12 bold}');

$mw->Photo('image1', -file => Tk->findINC("Xcamel.gif"));

my $l = $mw->Label(Name => "l", qw(-text Label))->pack;
my $b = $mw->Button(Name => "b", qw(-text Button))->pack;
my $c = $mw->Checkbutton(Name => "c", qw(-text Checkbutton))->pack;
my $r = $mw->Radiobutton(Name => "r", qw(-text Radiobutton))->pack;
$mw->update;

use constant SKIP_CGET    => 5;
use constant SKIP_CONF    => 6;
use constant SKIP_ERROR   => 7;
use constant SKIP_RESTORE => 8;

sub test_command { }
my $test_command = $mw->Callback(\&test_command);
my $test_font = $mw->fontCreate(-family => 'helvetica', -size => 10);
my $test_variable = "foo";

my @tests =
    (
     ['-activebackground', '#012345', '#012345', 'non-existent',
      q{unknown color name "non-existent"}, qw{1 1 1 1}
     ],
     ['-activeforeground', '#ff0000', '#ff0000', 'non-existent',
      q{unknown color name "non-existent"}, qw{1 1 1 1}
     ],
     ['-anchor', 'nw', 'nw', 'bogus',
      q{bad anchor "bogus": must be n, ne, e, se, s, sw, w, nw, or center},
      qw{1 1 1 1}
     ],
     ['-background', '#ff0000', '#ff0000', 'non-existent',
      q{unknown color name "non-existent"}, qw{1 1 1 1}
     ],
     ['-bd', '4', '4', 'badValue', q{bad screen distance "badValue"}, qw{1 1 1 1}],
     ['-bg', '#ff0000', '#ff0000', 'non-existent', q{unknown color name "non-existent"},
      qw{1 1 1 1}
     ],
     ['-bitmap', 'questhead', 'questhead', 'badValue', q{bitmap "badValue" not defined},
      qw{1 1 1 1}
     ],
     ['-borderwidth', '1.3', '1.3', 'badValue', q{bad screen distance "badValue"}, qw{1 1 1 1}],
     ['-command', $test_command, $test_command, q{}, q{}, qw{0 1 1 1}],
     ['-compound', 'left', 'left', 'bogus',
      q{bad compound "bogus": must be bottom, center, left, none, right, or top},
      qw{1 1 1 1}],
     ['-cursor', 'arrow', 'arrow', 'badValue', q{bad cursor spec "badValue"}, qw{1 1 1 1}],
     ['-default', 'active', 'active', 'huh?',
      q{bad default "huh?": must be active, disabled, or normal},
      qw{0 1 0 0}],
     ['-disabledforeground', '#00ff00', '#00ff00', 'xyzzy', q{unknown color name "xyzzy"},
      qw{1 1 1 1}],
     ['-fg', '#110022', '#110022', 'bogus', q{unknown color name "bogus"}, qw{1 1 1 1}],
     ['-font', $test_font, $test_font, q{}, q{font "" doesn't exist}, qw{1 1 1 1}],
     ['-foreground', '#110022', '#110022', 'bogus', q{unknown color name "bogus"}, qw{1 1 1 1}],
     ['-height', '18', '18', '20x', q{bad screen distance "20x"}, qw{1 1 1 1}], # original error 'was expected integer but got "20.0"', but Perl does not care about float vs. integer
     ['-highlightbackground', '#112233', '#112233', 'ugly', q{unknown color name "ugly"},
      qw{1 1 1 1}],
     ['-highlightcolor', '#110022', '#110022', 'bogus', q{unknown color name "bogus"},
      qw{1 1 1 1}],
     ['-highlightthickness', '6m', '6m', 'badValue', q{bad screen distance "badValue"},
      qw{1 1 1 1}],
     ['-image', 'image1', 'image1', 'bogus', q{image "bogus" doesn't exist}, qw{1 1 1 1}],
     ['-indicatoron', 'yes', '1', '', '', # XXX no error in Perl/Tk: 'no_way', q{expected boolean value but got "no_way"},
      qw{0 0 1 1}],
     ['-justify', 'right', 'right', 'bogus',
      q{bad justification "bogus": must be left, right, or center},
      qw{1 1 1 1}],
     ['-offrelief', 'flat', 'flat', '1.5',
      q{bad relief "1.5": must be flat, groove, raised, ridge, solid, or sunken},
      qw{0 0 1 1}],
     ['-offvalue', 'lousy', 'lousy', q{}, q{}, qw{0 0 1 0}],
     ['-onvalue', 'fantastic', 'fantastic', q{}, q{}, qw{0 0 1 0}],
     ['-overrelief', '', '', '1.5',
      q{bad relief "1.5": must be flat, groove, raised, ridge, solid, or sunken},
      qw(0 1 1 1)],
     ['-padx', '12m', '12m', '420x', q{bad screen distance "420x"}, qw{1 1 1 1}],
     ['-pady', '12m', '12m', '420x', q{bad screen distance "420x"}, qw{1 1 1 1}],
     ['-repeatdelay', '100', '100', 'foo', q{'foo' isn't numeric}, qw{0 1 0 0}], # original error 'expected integer but got "foo"'
     ['-repeatinterval', '100', '100', 'foo', q{'foo' isn't numeric}, qw{0 1 0 0}], # original error 'expected integer but got "foo"'
     ['-relief', 'flat', 'flat', '1.5',
      q{bad relief "1.5": must be flat, groove, raised, ridge, solid, or sunken},
      qw{1 1 1 1}],
     ['-selectcolor', '#110022', '#110022', 'bogus', q{unknown color name "bogus"}, qw{0 0 1 1}],
     ['-selectimage', 'image1', 'image1', 'bogus', q{image "bogus" doesn't exist}, qw{0 0 1 1}],
     ['-state', 'normal', 'normal', 'bogus',
      q{bad state "bogus": must be active, disabled, or normal},
      qw{1 1 1 1}],
     ['-takefocus', 'any string', 'any string', q{}, q{}, qw{1 1 1 1}],
     ['-text', 'Sample text', q{Sample text}, q{}, q{}, qw{1 1 1 1}],
     ['-textvariable', \$test_variable, \$test_variable, q{}, q{}, qw{1 1 1 1}],
## not in 8.4
#      ['-tristateimage', 'image1', 'image1', 'bogus', q{image "bogus" doesn't exist},
#       qw{0 0 1 1}],
#      ['-tristatevalue', 'unknowable', 'unknowable', q{}, q{}, qw{0 0 1 1}],
     ['-underline', '5', '5', '3p', q{'3p' isn't numeric}, qw{1 1 1 1}], # original error 'expected integer but got "3p"'
     ['-value', 'anyString', 'anyString', q{}, q{}, qw{0 0 0 1}],
     ['-width', '402', '402', '', '', # no error in Perl/Tk: '3p', q{'3p' isn't numeric},
      qw{1 1 1 1}],
     ['-wraplength', '100', '100', '6x', q{bad screen distance "6x"}, qw{1 1 1 1}],
 );

foreach my $test (@tests) {
    my($name, $value, $okResult, $badValue, $badResult, @classes) = @$test;
    for my $i (0 .. 3) {
	my $w_name = [qw(l b c r)]->[$i];
	my $hasOption = $classes[$i];
	my $w = $mw->Widget(".$w_name");
	die "Cannot get widget from .$w_name" if !$w;
	my $classname = $w->class;
	if ($hasOption) {
	    $w->configure($name, $value);
	    is(($w->configure($name))[4], $okResult, "configuration option $name for $classname");

	    if ($badValue ne "") {
		eval {
		    $w->configure($name, $badValue);
		};
		like($@, qr{\Q$badResult}, "bad value for configuration option $name for $classname");
	    }
	} else {
	   eval {
	       $w->configure($name, $value);
	   };
	   like($@, qr{^\Qunknown option "$name"}, "unknown configuration option $name for $classname");
       }
	       
    }
}

# Additional check to make sure that -selectcolor may be empty in
# checkbox widgets
$c->configure(-selectcolor => '');
pass('Empty selectcolor in Checkbutton');

for my $class (qw(Label Button Checkbutton Radiobutton)) {
    my $x = $mw->$class;
    is($x->class, $class, "ButtonCreate procedure - setting label $class");
}

{
    $mw->optionAdd('*funny.background', 'bogus');
    eval {
	$mw->Button(Name => "funny");
    };
    like($@, qr{\Qunknown color name "bogus" at},
	 "ButtonCreate procedure - error in default option value");
}

{
    my $w = eval {
	$mw->Button(qw(-gorp foo));
    };
    like($@, qr{\Qunknown option "-gorp"}, "ButtonCreate procedure - option error");
    ok(!Tk::Exists($w), "Button did not get created");
}

__END__

test button-4.1 {ButtonWidgetCmd - too few arguments} {
    list [catch {.b} msg] $msg
} {1 {wrong # args: should be ".b option ?arg arg ...?"}}
test button-4.2 {ButtonWidgetCmd - bad option name} {
    list [catch {.b c} msg] $msg
} {1 {ambiguous option "c": must be cget, configure, flash, or invoke}}
test button-4.3 {ButtonWidgetCmd - bad option name} {
    list [catch {.b bogus} msg] $msg
} {1 {bad option "bogus": must be cget, configure, flash, or invoke}}
test button-4.4 {ButtonWidgetCmd procedure, "cget" option} {
    list [catch {.b cget a b} msg] $msg
} {1 {wrong # args: should be ".b cget option"}}
test button-4.5 {ButtonWidgetCmd procedure, "cget" option} {
    list [catch {.b cget -gorp} msg] $msg
} {1 {unknown option "-gorp"}}
test button-4.6 {ButtonWidgetCmd procedure, "cget" option} {
    .b configure -highlightthickness 3
    .b cget -highlightthickness
} {3}
test button-4.7 {ButtonWidgetCmd procedure, "cget" option} {
    catch {.l cget -disabledforeground}
} {0}
test button-4.8 {ButtonWidgetCmd procedure, "cget" option} {
    catch {.b cget -disabledforeground}
} {0}
test button-4.9 {ButtonWidgetCmd procedure, "cget" option} {
    list [catch {.b cget -variable} msg] $msg
} {1 {unknown option "-variable"}}
test button-4.10 {ButtonWidgetCmd procedure, "cget" option} {
    catch {.c cget -variable}
} {0}
test button-4.11 {ButtonWidgetCmd procedure, "cget" option} {
    list [catch {.c cget -value} msg] $msg
} {1 {unknown option "-value"}}
test button-4.12 {ButtonWidgetCmd procedure, "cget" option} {
    catch {.r cget -value}
} {0}
test button-4.13 {ButtonWidgetCmd procedure, "cget" option} {
    list [catch {.r cget -onvalue} msg] $msg
} {1 {unknown option "-onvalue"}}
test button-4.14 {ButtonWidgetCmd procedure, "configure" option} {
    llength [.c configure]
} {41}
test button-4.15 {ButtonWidgetCmd procedure, "configure" option} {
    list [catch {.b configure -gorp} msg] $msg
} {1 {unknown option "-gorp"}}
test button-4.16 {ButtonWidgetCmd procedure, "configure" option} {
    list [catch {.b co -bg #ffffff -fg} msg] $msg
} {1 {value for "-fg" missing}}
test button-4.17 {ButtonWidgetCmd procedure, "configure" option} {
    .b configure -fg #123456
    .b configure -bg #654321
    lindex [.b configure -fg] 4
} {#123456}
.c configure -variable value -onvalue 1 -offvalue 0
.r configure -variable value2 -value red
test button-4.18 {ButtonWidgetCmd procedure, "deselect" option} {
    list [catch {.c deselect foo} msg] $msg
} {1 {wrong # args: should be ".c deselect"}}
test button-4.19 {ButtonWidgetCmd procedure, "deselect" option} {
    list [catch {.l deselect} msg] $msg
} {1 {bad option "deselect": must be cget or configure}}
test button-4.20 {ButtonWidgetCmd procedure, "deselect" option} {
    list [catch {.b deselect} msg] $msg
} {1 {bad option "deselect": must be cget, configure, flash, or invoke}}
test button-4.21 {ButtonWidgetCmd procedure, "deselect" option} {
    set value 1
    .c d
    set value
} {0}
test button-4.22 {ButtonWidgetCmd procedure, "deselect" option} {
    set value2 green
    .r deselect
    set value2
} {green}
test button-4.23 {ButtonWidgetCmd procedure, "deselect" option} {
    set value2 red
    .r deselect
    set value2
} {}
test button-4.24 {ButtonWidgetCmd procedure, "deselect" option} -body {
    set value 1
    trace variable value w bogusTrace
    set result [list [catch {.c deselect} msg] $msg $errorInfo $value]
    trace vdelete value w bogusTrace
    set result
} -match glob -result {1 {can't set "value": trace aborted} {*trace aborted
    while executing
*
".c deselect"} 0}
test button-4.25 {ButtonWidgetCmd procedure, "deselect" option} -body {
    set value2 red
    trace variable value2 w bogusTrace
    set result [list [catch {.r deselect} msg] $msg $errorInfo $value2]
    trace vdelete value2 w bogusTrace
    set result
} -match glob -result {1 {can't set "value2": trace aborted} {*trace aborted
    while executing
*
".r deselect"} {}}
test button-4.26 {ButtonWidgetCmd procedure, "flash" option} {
    list [catch {.b flash foo} msg] $msg
} {1 {wrong # args: should be ".b flash"}}
test button-4.27 {ButtonWidgetCmd procedure, "flash" option} {
    list [catch {.l flash} msg] $msg
} {1 {bad option "flash": must be cget or configure}}
test button-4.28 {ButtonWidgetCmd procedure, "flash" option} {
    list [catch {.b flash} msg] $msg
} {0 {}}
test button-4.29 {ButtonWidgetCmd procedure, "flash" option} {
    list [catch {.c flash} msg] $msg
} {0 {}}
test button-4.30 {ButtonWidgetCmd procedure, "flash" option} {
    list [catch {.r f} msg] $msg
} {0 {}}
test button-4.31 {ButtonWidgetCmd procedure, "invoke" option} {
    list [catch {.b invoke foo} msg] $msg
} {1 {wrong # args: should be ".b invoke"}}
test button-4.32 {ButtonWidgetCmd procedure, "invoke" option} {
    list [catch {.l invoke} msg] $msg
} {1 {bad option "invoke": must be cget or configure}}
test button-4.33 {ButtonWidgetCmd procedure, "invoke" option} {
    .b configure -command {set x invoked}
    set x "not invoked"
    .b invoke
    set x
} {invoked}
test button-4.34 {ButtonWidgetCmd procedure, "invoke" option} {
    .b configure -command {set x invoked} -state disabled
    set x "not invoked"
    .b invoke
    set x
} {not invoked}
test button-4.35 {ButtonWidgetCmd procedure, "invoke" option} {
    set value bogus
    .c configure -command {set x invoked} -variable value -onvalue 1 \
	    -offvalue 0
    set x "not invoked"
    .c invoke
    list $x $value
} {invoked 1}
test button-4.36 {ButtonWidgetCmd procedure, "invoke" option} {
    set value2 green
    .r configure -command {set x invoked} -variable value2 -value red
    set x "not invoked"
    .r i
    list $x $value2
} {invoked red}
test button-4.37 {ButtonWidgetCmd procedure, "select" option} {
    list [catch {.l select} msg] $msg
} {1 {bad option "select": must be cget or configure}}
test button-4.38 {ButtonWidgetCmd procedure, "select" option} {
    list [catch {.b select} msg] $msg
} {1 {bad option "select": must be cget, configure, flash, or invoke}}
test button-4.39 {ButtonWidgetCmd procedure, "select" option} {
    list [catch {.c select foo} msg] $msg
} {1 {wrong # args: should be ".c select"}}
test button-4.40 {ButtonWidgetCmd procedure, "select" option} {
    set value bogus
    .c configure -command {} -variable value -onvalue lovely -offvalue 0
    .c s
    set value
} {lovely}
test button-4.41 {ButtonWidgetCmd procedure, "select" option} {
    set value2 green
    .r configure -command {} -variable value2 -value red
    .r select
    set value2
} {red}
test button-4.42 {ButtonWidgetCmd procedure, "select" option} -body {
    set value2 yellow
    trace variable value2 w bogusTrace
    set result [list [catch {.r select} msg] $msg $errorInfo $value2]
    trace vdelete value2 w bogusTrace
    set result
} -match glob -result {1 {can't set "value2": trace aborted} {*trace aborted
    while executing
*
".r select"} red}
test button-4.43 {ButtonWidgetCmd procedure, "toggle" option} {
    list [catch {.l toggle} msg] $msg
} {1 {bad option "toggle": must be cget or configure}}
test button-4.44 {ButtonWidgetCmd procedure, "toggle" option} {
    list [catch {.b toggle} msg] $msg
} {1 {bad option "toggle": must be cget, configure, flash, or invoke}}
test button-4.45 {ButtonWidgetCmd procedure, "toggle" option} {
    list [catch {.r toggle} msg] $msg
} {1 {bad option "toggle": must be cget, configure, deselect, flash, invoke, or select}}
test button-4.46 {ButtonWidgetCmd procedure, "toggle" option} {
    list [catch {.c toggle foo} msg] $msg
} {1 {wrong # args: should be ".c toggle"}}
test button-4.47 {ButtonWidgetCmd procedure, "toggle" option} {
    set value bogus
    .c configure -command {} -variable value -onvalue sunshine -offvalue rain
    .c toggle
    set result $value
    .c toggle
    lappend result $value
    .c toggle
    lappend result $value
} {sunshine rain sunshine}
test button-4.48 {ButtonWidgetCmd procedure, "toggle" option} -body {
    .c configure -onvalue xyz -offvalue abc
    set value xyz
    trace variable value w bogusTrace
    set result [list [catch {.c toggle} msg] $msg $errorInfo $value]
    trace vdelete value w bogusTrace
    set result
} -match glob -result {1 {can't set "value": trace aborted} {*trace aborted
    while executing
*
".c toggle"} abc}
test button-4.49 {ButtonWidgetCmd procedure, "toggle" option} -body {
    .c configure -onvalue xyz -offvalue abc
    set value abc
    trace variable value w bogusTrace
    set result [list [catch {.c toggle} msg] $msg $errorInfo $value]
    trace vdelete value w bogusTrace
    set result
} -match glob -result {1 {can't set "value": trace aborted} {*trace aborted
    while executing
*
".c toggle"} xyz}
test button-4.50 {ButtonWidgetCmd procedure, "toggle" option} {
    catch {unset value}; set value(1) 1;
    set result [list [catch {.c toggle} msg] $msg $errorInfo]
    unset value;
    set result
} {1 {can't set "value": variable is array} {can't set "value": variable is array
    while executing
".c toggle"}}

test button-5.1 {DestroyButton procedure} testImageType {
    image create test image1
    button .b1 -image image1
    button .b2 -fg #ff0000 -text "Button 2"
    button .b3 -state active -text "Button 3"
    button .b4 -disabledforeground #0000ff -state disabled -text "Button 4"
    checkbutton .b5 -variable x -text "Checkbutton 5"
    set x 1
    pack .b1 .b2 .b3 .b4 .b5
    update
    deleteWindows
} {}

test button-6.1 {ConfigureButton - textvariable trace} {
    catch {destroy .b1}
    button .b1 -bd 4 -bg green
    catch {.b1 configure -bd 7 -bg green -fg bogus}
    list [catch {.b1 configure -bd 7 -bg red -fg bogus} msg] \
	    $msg [.b1 cget -bd] [.b1 cget -bg]
} {1 {unknown color name "bogus"} 4 green}
test button-6.2 {ConfigureButton - textvariable trace} {
    catch {destroy .b1}
    set x From-x
    set y From-y
    button .b1 -textvariable x
    .b1 configure -textvariable y
    set x New
    lindex [.b1 configure -text] 4
} {From-y}
test button-6.2a {ConfigureButton - variable traces} {
    catch {destroy .b1}
    catch {unset x}
    checkbutton .b1 -variable x
    set x 1
    set y 1
    .b1 configure -textvariable y
    set x 0
    .b1 toggle
    set y
} {1}
test button-6.3 {ConfigureButton - image handling} testImageType {
    catch {destroy .b1}
    eval image delete [image names]
    image create test image1
    image create test image2
    button .b1 -image image1
    image delete image1
    .b1 configure -image image2
    image names
} {image2}
test button-6.5 {ConfigureButton - default value for variable} {
    catch {destroy .b1}
    checkbutton .b1
    .b1 cget -variable
} {b1}
test button-6.6 {ConfigureButton - setting selected state from variable} {
    catch {destroy .b1}
    set x 0
    set y Shiny
    checkbutton .b1 -variable x
    .b1 configure -variable y -onvalue Shiny
    .b1 toggle
    set y
} 0
test button-6.7 {ConfigureButton - setting selected state from variable} {
    catch {destroy .b1}
    catch {unset x}
    checkbutton .b1 -variable x -offvalue Bogus
    set x
} Bogus
test button-6.8 {ConfigureButton - setting selected state from variable} {
    catch {destroy .b1}
    catch {unset x}
    radiobutton .b1 -variable x
    set x
} {}
test button-6.9 {ConfigureButton - error in setting variable} {
    catch {destroy .b1}
    catch {unset x}
    trace variable x w bogusTrace
    set result [list [catch {radiobutton .b1 -variable x} msg] $msg]
    trace vdelete x w bogusTrace
    set result
} {1 {can't set "x": trace aborted}}
test button-6.10 {ConfigureButton - bad image name} {
    catch {destroy .b1}
    list [catch {button .b1 -image bogus} msg] $msg
} {1 {image "bogus" doesn't exist}}
test button-6.11 {ConfigureButton - setting variable from current text value} {
    catch {destroy .b1}
    catch {unset x}
    button .b1 -textvariable x -text "Button 1"
    set x
} {Button 1}
test button-6.12 {ConfigureButton - using current value of variable} {
    catch {destroy .b1}
    set x Override
    button .b1 -textvariable x -text "Button 1"
    set x
} {Override}
test button-6.13 {ConfigureButton - variable handling} {
    catch {destroy .b1}
    catch {unset x}
    trace variable x w bogusTrace
    set result [list [catch {radiobutton .b1 -text foo -textvariable x} msg] \
	    $msg $x]
    trace vdelete x w bogusTrace
    set result
} {1 {can't set "x": trace aborted} foo}
test button-6.14 {ConfigureButton - -width option} {
    catch {destroy .b1}
    button .b1 -text "Button 1"
    list [catch {.b1 configure -width 1i} msg] $msg $errorInfo
} {1 {expected integer but got "1i"} {expected integer but got "1i"
    (processing -width option)
    invoked from within
".b1 configure -width 1i"}}
test button-6.15 {ConfigureButton - -height option} {
    catch {destroy .b1}
    button .b1 -text "Button 1"
    list [catch {.b1 configure -height 0.5c} msg] $msg $errorInfo
} {1 {expected integer but got "0.5c"} {expected integer but got "0.5c"
    (processing -height option)
    invoked from within
".b1 configure -height 0.5c"}}
test button-6.16 {ConfigureButton - -width option} {
    catch {destroy .b1}
    button .b1 -bitmap questhead
    list [catch {.b1 configure -width abc} msg] $msg $errorInfo
} {1 {bad screen distance "abc"} {bad screen distance "abc"
    (processing -width option)
    invoked from within
".b1 configure -width abc"}}
test button-6.17 {ConfigureButton - -height option} testImageType {
    catch {destroy .b1}
    eval image delete [image names]
    image create test image1
    button .b1 -image image1
    list [catch {.b1 configure -height 0.5x} msg] $msg $errorInfo
} {1 {bad screen distance "0.5x"} {bad screen distance "0.5x"
    (processing -height option)
    invoked from within
".b1 configure -height 0.5x"}}
test button-6.18 {ConfigureButton - computing geometry} {nonPortable fonts} {
    catch {destroy .b1}
    button .b1 -text "Sample text" -width 10 -height 2
    pack .b1
    set result "[winfo reqwidth .b1] [winfo reqheight .b1]"
    .b1 configure -bitmap questhead
    lappend result [winfo reqwidth .b1] [winfo reqheight .b1]
} {102 46 20 12}
test button-6.19 {ConfigureButton - computing geometry} {
    catch {destroy .b1}
    button .b1 -text "Button 1"
    set old [winfo reqwidth .b1]
    .b1 configure -text "Much longer text"
    set new [winfo reqwidth .b1]
    expr $old == $new
} {0}

test button-7.1 {ButtonEventProc procedure} {
    catch {destroy .b1}
    button .b1 -text "Test Button" -command {
	destroy .b1
	set x [list [winfo exists .b1] [info commands .b1]]
    }
    .b1 invoke
    set x
} {0 {}}
test button-7.2 {ButtonEventProc procedure} {
    deleteWindows
    button .b1 -bg #543210
    rename .b1 .b2
    set x {}
    lappend x [winfo children .]
    lappend x [.b2 cget -bg]
    destroy .b1
    lappend x [info command .b*] [winfo children .]
} {.b1 #543210 {} {}}

test button-8.1 {ButtonCmdDeletedProc procedure} {
    deleteWindows
    button .b1
    rename .b1 {}
    list [info command .b*] [winfo children .]
} {{} {}}

test button-9.1 {TkInvokeButton procedure} {
    catch {destroy .b1}
    set x 0
    checkbutton .b1 -variable x
    set result $x
    .b1 invoke
    lappend result $x
    .b1 invoke
    lappend result $x
} {0 1 0}
test button-9.2 {TkInvokeButton procedure} {
    catch {destroy .b1}
    set x 0
    checkbutton .b1 -variable x
    trace variable x w bogusTrace
    set result [list [catch {.b1 invoke} msg] $msg $x]
    trace vdelete x w bogusTrace
    set result
} {1 {can't set "x": trace aborted} 1}
test button-9.3 {TkInvokeButton procedure} {
    catch {destroy .b1}
    set x 1
    checkbutton .b1 -variable x
    trace variable x w bogusTrace
    set result [list [catch {.b1 invoke} msg] $msg $x]
    trace vdelete x w bogusTrace
    set result
} {1 {can't set "x": trace aborted} 0}
test button-9.4 {TkInvokeButton procedure} {
    catch {destroy .b1}
    set x 0
    radiobutton .b1 -variable x -value red
    set result $x
    .b1 invoke
    lappend result $x
    .b1 invoke
    lappend result $x
} {0 red red}
test button-9.5 {TkInvokeButton procedure} -body {
    catch {destroy .b1}
    radiobutton .b1 -variable x -value red
    set x green
    trace variable x w bogusTrace
    set result [list [catch {.b1 invoke} msg] $msg $errorInfo $x]
    trace vdelete x w bogusTrace
    set result
} -match glob -result {1 {can't set "x": trace aborted} {*trace aborted
    while executing
*
".b1 invoke"} red}
test button-9.6 {TkInvokeButton procedure} {
    deleteWindows
    set result untouched
    button .b1 -command {set result invoked}
    list [catch {.b1 invoke} msg] $msg $result
} {0 invoked invoked}
test button-9.7 {TkInvokeButton procedure} {
    deleteWindows
    set result untouched
    set x 0
    checkbutton .b1 -variable x -command {set result "invoked $x"}
    list [catch {.b1 invoke} msg] $msg $result
} {0 {invoked 1} {invoked 1}}
test button-9.8 {TkInvokeButton procedure} {
    deleteWindows
    set result untouched
    set x 0
    radiobutton .b1 -variable x -value red -command {set result "invoked $x"}
    list [catch {.b1 invoke} msg] $msg $result
} {0 {invoked red} {invoked red}}

test button-10.1 {ButtonVarProc procedure} {
    deleteWindows
    set x 1
    checkbutton .b1 -variable x
    unset x
    set result [info exists x]
    .b1 toggle
    lappend result $x
    set x 0
    .b1 toggle
    lappend result $x
} {0 1 1}
test button-10.2 {ButtonVarProc procedure} {
    deleteWindows
    set x 0
    checkbutton .b1 -variable x
    set x 44
    .b1 toggle
    set x
} {1}
test button-10.3 {ButtonVarProc procedure} {
    deleteWindows
    set x 1
    checkbutton .b1 -variable x
    set x 44
    .b1 toggle
    set x
} {1}
test button-10.4 {ButtonVarProc procedure} {
    deleteWindows
    set x 0
    checkbutton .b1 -variable x
    set x 1
    .b1 toggle
    set x
} {0}
test button-10.5 {ButtonVarProc procedure} {
    deleteWindows
    set x 1
    checkbutton .b1 -variable x
    set x 1
    .b1 toggle
    set x
} {0}
test button-10.6 {ButtonVarProc procedure} {
    deleteWindows
    set x 0
    checkbutton .b1 -variable x
    set x 0
    .b1 toggle
    set x
} {1}
test button-10.7 {ButtonVarProc procedure} {
    deleteWindows
    set x 1
    checkbutton .b1 -variable x
    set x 0
    .b1 toggle
    set x
} {1}
test button-10.8 {ButtonVarProc procedure, can't read variable} {
    # This test does nothing but produce a core dump if there's a prbblem.
    deleteWindows
    catch {unset a}
    checkbutton .b1 -variable a
    unset a
    set a(32) 0
    unset a
} {}

test button-11.1 {ButtonTextVarProc procedure} {
    deleteWindows
    set x Label
    button .b1 -textvariable x
    unset x
    set result [list $x [lindex [.b1 configure -text] 4]]
    set x New
    lappend result [lindex [.b1 configure -text] 4]
} {Label Label New}
test button-11.2 {ButtonTextVarProc procedure} {
    deleteWindows
    # Windows buttons have a default min width, so we have to
    # set this to be longer to force the wider button.
    set x ExtraLongLabel
    button .b1 -textvariable x
    set old [winfo reqwidth .b1]
    set x New
    set new [winfo reqwidth .b1]
    list [lindex [.b1 configure -text] 4] [expr $old == $new]
} {New 0}

test button-12.1 {ButtonImageProc procedure} testImageType {
    deleteWindows
    eval image delete [image names]
    image create test image1
    label .b1 -image image1 -padx 0 -pady 0 -bd 0
    pack .b1
    set result "[winfo reqwidth .b1] [winfo reqheight .b1]"
    image1 changed 0 0 0 0 80 100
    lappend result [winfo reqwidth .b1] [winfo reqheight .b1]
} {30 15 80 100}

deleteWindows
set l [interp hidden]

test button-13.1 {button widget vs hidden commands} {
    catch {destroy .b}
    button .b -text hello
    interp hide {} .b
    destroy .b
    list [winfo children .] [interp hidden]
} [list {} $l]

deleteWindows

test button-14.1 {size behaviouor} {
    set res {}
    foreach class {label button radiobutton checkbutton} {
	eval destroy [winfo children .]

	$class .a -text Hej 
	$class .b -text Hej -width 10 -height 1
	$class .c -text "" -width 10 -height 1

	for {set t 0} {$t < 2} {incr t} {
	    set res2 {}
	    # With -width, width should not be affected by text change
	    lappend res2 [expr {[winfo reqwidth .b] == [winfo reqwidth .c]}]
	    # With -height, height should not be affected by text change
	    lappend res2 [expr {[winfo reqheight .b] == [winfo reqheight .c]}]
	    # A one line text should be as high as -height 1
	    lappend res2 [expr {[winfo reqheight .a] == [winfo reqheight .b]}]
	    lappend res $res2
	    
	    # Do the second round with another font
	    .a configure -font "Arial 20"
	    .b configure -font "Arial 20"
	    .c configure -font "Arial 20"
	}
    }
    set res
} {{1 1 1} {1 1 1} {1 1 1} {1 1 1} {1 1 1} {1 1 1} {1 1 1} {1 1 1}}

deleteWindows

option clear

# cleanup
cleanupTests
return

__END__

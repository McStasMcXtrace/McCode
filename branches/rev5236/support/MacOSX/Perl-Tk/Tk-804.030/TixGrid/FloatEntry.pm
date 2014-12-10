# Tranlation of FloatEnt.tcl in Tix4.1

# TODO/IDEA:
#	o extract a widget (SimpleEntry?) without post/unpost methods
#	  and derive FloatEntry fron this widget.

package Tk::FloatEntry;
use strict;

BEGIN
  {
    use vars '$DEBUG';
    $DEBUG = (defined($ENV{USER}) and $ENV{USER} eq 'achx') ? 1 : 0;
    print STDERR "tixGrid: debug = $DEBUG\n" if $DEBUG;
  }

require Tk;
require Tk::Widget;
require Tk::Derived;
require Tk::Entry;

use vars qw($VERSION);
$VERSION = '4.004'; # $Id: //depot/Tkutf8/TixGrid/FloatEntry.pm#4 $

use base  qw(Tk::Derived Tk::Entry);

Construct Tk::Widget 'FloatEntry';

sub ClassInit
  {
    my ($class, $mw) = @_;
    $class->SUPER::ClassInit($mw);
    $mw->bind($class, '<Return>', 'invoke');
    $mw->bind($class, '<FocusIn>', 'FocusIn');
    $class;
  }

sub Populate
  {
    my ($e, $args) = @_;
    $e->ConfigSpecs(
	-value	 =>            ['METHOD',   'value',              'Value',              undef],
	-highlightthickness => [$e,         'highlightThickness', 'HighlightThickness', 0    ],
	-command =>            ['CALLBACK', 'command',            'Command',            undef],
	);
    print "FloatEntry Init: $e\n" if $DEBUG;
    $e;
  }

## option method

sub value
  {
    my $e = shift;
    unless (@_)
      {
	return $e->get
      }
    $e->delete(0,'end');
    $e->insert(0,$_[0]);
    $e->selection('from', 0);
    $e->selection('to', 'end');

  }

## public methods

sub invoke
  {
    my ($e) = @_;
    $e->Callback('-command', $e->get);
  }

sub post
  {
    my ($e, $x, $y, $dx, $dy) = @_;

    $dx = $e->reqwidth  unless defined $dx;
    $dy = $e->reqheight unless defined $dy;

    $e->place('-x'=>$x, '-y'=>$y, -width=>$dx, -height=>$dy, -bordermode=>'ignore');
    $e->raise;
    $e->focus;
  }

sub unpost
  {
    my ($e) = @_;
    $e->place('forget');
  }

## bindings

sub FocusIn
  {
    my ($e) = @_;

    # FIX: xxx only if entry has not already focus
      {
	$e->focus;
	$e->selection('from', 0);
	$e->selection('to', 'end');
	$e->icursor('end');
      }
  }

1;
__END__


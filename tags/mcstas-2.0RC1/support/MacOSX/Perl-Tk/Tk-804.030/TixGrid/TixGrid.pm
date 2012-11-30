
# TODO:
#
#	o How to get into state 's0' 'b0' so cursor keys start
#	  working (compare with Tk/Widget  XYscrollBind
#	o the options -browsecmd and -command callback are not
#	  not implemented (as in Tix)
#	o privateData 'state' used only once (check again Grid.tcl)
#	o FloatEntry 'sometimes not activeted immediately on selection
#	o check also Leave Binding. Looks like entry does get unpost'ed

package Tk::TixGrid;

BEGIN
  {
    use vars '$DEBUG';
    $DEBUG = (defined($ENV{USER}) and $ENV{USER} eq 'ach') ? 1 : 0;
    print STDERR "tixGrid: debug = $DEBUG\n" if $DEBUG;
  }

use strict;
use vars qw($VERSION);
$VERSION = '4.010'; # sprintf '4.%03d', q$Revision: #9 $ =~ /\D(\d+)\s*$/;

use Tk qw(Ev $XS_VERSION);
use Tk::Widget;
use Carp;

# carp "\n".__PACKAGE__.' is deprecated' unless defined($Test::ntest);

use base  'Tk::Widget';

Construct Tk::Widget 'TixGrid';

bootstrap Tk::TixGrid;

sub Tk_cmd { \&Tk::tixGrid }

sub Tk::Widget::SrclTixGrid { shift->Scrolled('TixGrid' => @_) }

Tk::Methods qw(anchor bdtype delete entrycget entryconfigure format index
               move set size unset xview yview
               dragsite dropsite geometryinfo info
               nearest see selection sort );

use Tk::Submethods
		(
		'anchor'	=> [ qw(get    set) ],
		'delete'	=> [ qw(column row) ],
		'info'		=> [ qw(bbox  exists anchor) ],
		'move'		=> [ qw(column row) ],
		'selection'	=> [ qw(adjust clear  includes set) ],
		'size'		=> [ qw(column row) ],
		'format'	=> [ qw(grid   border) ],
		);

# edit subcommand is special. It justs invokes tcl code:
#
#	edit set x y  ->   tixGrid:EditCell $w, x, y
#	edit apply	-> tixGrid:EditApply

# xxx Create an edit sub?
# sub edit { .... }

sub editSet
  {
    die "wrong args. Should be \$w->editSet(x,y)\n" unless @_ == 3;
    my ($w, $x, $y) = @_;
    $w->EditCell($x, $y);
  }

sub editApply
  {
    die "wrong args. Should be \$w->editApply()\n" unless @_ == 1;
    my ($w) = @_;
    $w->EditApply()
  }


####################################################
##
## For button 2 scrolling. So TixGrid has 'standard'
## standard scrolling interface
##

#sub scanMark
#  {
#    die "wrong # args: \$w->scanMark(x,y)\n" unless @_ == 3;
#    my ($w) = @_;
#    $w->{__scanMarkXY__} = [ @_[1,2] ];
#    return "";
#  }
#
#sub scanDragto
#  {
#    die "wrong # args: \$w->scanDragto(x,y)\n" unless @_ == 3;
#    my ($w, $x, $y) = @_;
#    my ($ox, $oy) = @{ $w->{__scanMarkXY__} };
#
#  #...
#
#    return "";
#  }

### end button 2 scrolling stuff ####################


# Grid.tcl --
#
# 	This file defines the default bindings for Tix Grid widgets.
#
# Copyright (c) 1996, Expert Interface Technologies
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# Bindings translated to perl/Tk by Achim Bohnet <ach@mpe.mpg.de>

sub ClassInit
  {
    my ($class, $mw) = @_;
    $class->SUPER::ClassInit($mw);

    $mw->XYscrollBind($class);

    ##
    ## Button bindings
    ##

    $mw->bind($class, '<ButtonPress-1>',	['Button_1',		Ev('x'), Ev('y')]);
    $mw->bind($class, '<Shift-ButtonPress-1>',	['Shift_Button_1',	Ev('x'), Ev('y')]);
    $mw->bind($class, '<Control-ButtonPress-1>',['Control_Button_1',	Ev('x'), Ev('y')]);
    $mw->bind($class, '<ButtonRelease-1>',	['ButtonRelease_1',	Ev('x'), Ev('y')]);
    $mw->bind($class, '<Double-ButtonPress-1>', ['Double_1',		Ev('x'), Ev('y')]);
    $mw->bind($class, '<B1-Motion>','Button_Motion');
    $mw->bind($class, '<Control-B1-Motion>','Control_Button_Motion');
    $mw->bind($class, '<B1-Leave>','Button_Leave');
    $mw->bind($class, '<Double-ButtonPress-1>', ['Double_1',		Ev('x'), Ev('y')]);
    $mw->bind($class, '<B1-Enter>',		['B1_Enter',		Ev('x'), Ev('y')]);
    $mw->bind($class, '<Control-B1-Leave>','Control_Button_Leave');
    $mw->bind($class, '<Control-B1-Enter>',	['Control_B1_Enter',	Ev('x'), Ev('y')]);

    ##
    ## Keyboard bindings
    ##

    $mw->bind($class, '<Up>',		['DirKey',	'up'	]);
    $mw->bind($class, '<Down>',		['DirKey',	'down'	]);
    $mw->bind($class, '<Left>',		['DirKey',	'left'	]);
    $mw->bind($class, '<Right>',	['DirKey',	'right'	]);

    $mw->PriorNextBind($class);

    $mw->bind($class, '<Return>',	'Return');
    $mw->bind($class, '<space>',	'Space'	);

    return $class;
  }

#----------------------------------------------------------------------
#
#
#			 Mouse bindings
#
#
#----------------------------------------------------------------------

sub Button_1
  {
    my $w = shift;

    return if $w->cget('-state') eq 'disabled';
    $w->SetFocus;
    $w->ChgState(@_,
		[
		'0'=>'1',
		]
	 	);
  }

sub Shift_Button_1
  {
    my $w = shift;

    return if $w->cget('-state') eq 'disabled';
    $w->SetFocus;

#    $w->ChgState(@_,
#		[
#		]
#		);
  }

sub Control_Button_1
  {
    my $w = shift;

    return if $w->cget('-state') eq 'disabled';
    $w->SetFocus;

    $w->ChgState(@_,
		[
		's0'	=> 's1',
		'b0'    => 'b1',
		'm0'	=> 'm1',
		'e0'	=> 'e10',
		]
	 	);
  }

sub ButtonRelease_1
  {
    shift->ChgState(@_,
		[
		'2'	=> '5',
		'4'	=> '3',
		]
		);
  }

sub B1_Motion
  {
    shift->ChgState(@_,
		[
		'2'	=> '4',
		'4'	=> '4',
		]
		);
  }


sub Control_B1_Motion
  {
    shift->ChgState(@_,
		[
		's2'	=> 's4',
		's4'	=> 's4',
		'b2'	=> 'b4',
		'b4'	=> 'b4',
		'm2'	=> 'm4',
		'm5'	=> 'm4',
		]
		);
  }


sub Double_1
  {
    shift->ChgState(@_,
		[
		's0'	=> 's7',
		'b0'	=> 'b7',
		]
		);
  }


sub B1_Leave
  {
    shift->ChgState(@_,
		[
		's2'	=> 's5',
		's4'	=> 's5',
		'b2'	=> 'b5',
		'b4'	=> 'b5',
		'm2'	=> 'm8',
		'm5'	=> 'm8',
		'e2'	=> 'e8',
		'e5'	=> 'e8',
		]
		);
  }


sub B1_Enter
  {
    shift->ChgState(@_,
		[
		's5'	=> 's4',
		's6'	=> 's4',
		'b5'	=> 'b4',
		'b6'	=> 'b4',
		'm8'	=> 'm4',
		'm9'	=> 'm4',
		'e8'	=> 'e4',
		'e9'	=> 'e4',
		]
		);
  }


sub Control_B1_Leave
  {
    shift->ChgState(@_,
		[
		's2'	=> 's5',
		's4'	=> 's5',
		'b2'	=> 'b5',
		'b4'	=> 'b5',
		'm2'	=> 'm8',
		'm5'	=> 'm8',
		]
		);
  }


sub Control_B1_Enter
  {
    shift->ChgState(@_,
		[
		's5'	=> 's4',
		's6'	=> 's4',
		'b5'	=> 'b4',
		'b6'	=> 'b4',
		'm8'	=> 'm4',
		'm9'	=> 'm4',
		]
		);
  }


sub AutoScan
  {
    shift->ChgState(@_,
		[
		's5'	=> 's9',
		's6'	=> 's9',
		'b5'	=> 'b9',
		'b6'	=> 'b9',
		'm8'	=> 'm9',
		'm9'	=> 'm9',
		'e8'	=> 'm9',
		'e9'	=> 'm9',
		]
		);
  }

#----------------------------------------------------------------------
#
#
#			 Key bindings
#
#
#----------------------------------------------------------------------

sub DirKey
  {
    my ($w, $key) = @_;

    return if $w->cget('-state') eq 'disabled';

print STDERR "$w->DirKey($key)\n" if $DEBUG;
    $w->ChgState($key,
		[
		's0'	=> 's8',
		'b0'	=> 'b8',
		]
		);
  }


sub Return
  {
    my ($w) = @_;

    return if $w->cget('-state') eq 'disabled';

    $w->ChgState(
		[
		's0'	=> 's9',
		'b0'	=> 'b9',
		]
		);
  }


sub Space
  {
    my ($w) = @_;

    return if $w->cget('-state') eq 'disabled';

    $w->ChgState(
		[
		's0'	=> 's10',
		'b0'	=> 'b10',
		]
		);
  }


#----------------------------------------------------------------------
#
#			STATE MANIPULATION
#
#
#----------------------------------------------------------------------

sub GetState
  {
    my ($w) = @_;
    my $data = $w->privateData();
    $data->{state} = 0 unless exists $data->{state};
    return $data->{state};
}

sub Button_Motion
{
 my $w = shift;
 my $Ev = $w->XEvent;
 $Tk::x =  $Ev->x;
 $Tk::y =  $Ev->y;
 $Tk::X =  $Ev->X;
 $Tk::Y =  $Ev->Y;
 $w->B1_Motion($Tk::x, $Tk::y);
}


sub Control_Button_Motion
{
 my $w = shift;
 my $Ev = $w->XEvent;
 $Tk::x =  $Ev->x;
 $Tk::y =  $Ev->y;
 $Tk::X =  $Ev->X;
 $Tk::Y =  $Ev->Y;
 $w->Control_B1_Motion($Tk::x, $Tk::y);
}


sub Button_Leave
{
 my $w = shift;
 my $Ev = $w->XEvent;
 $Tk::x =  $Ev->x;
 $Tk::y =  $Ev->y;
 $Tk::X =  $Ev->X;
 $Tk::Y =  $Ev->Y;
 $w->B1_Leave();
}


sub Control_Button_Leave
{
 my $w = shift;
 my $Ev = $w->XEvent;
 $Tk::x =  $Ev->x;
 $Tk::y =  $Ev->y;
 $Tk::X =  $Ev->X;
 $Tk::Y =  $Ev->Y;
 $w->Control_B1_Leave();
}


sub SetState
  {
    my ($w, $state) = @_;
    $w->privateData()->{state} = $state;
  }

sub GoState
  {
    my ($w, $state) = (shift, shift);
    print STDERR 'Gostate:  ', $w->GetState, " --> $state, " if $DEBUG;
    $w->SetState($state);
    my $method = "GoState_$state";

    print STDERR 'args=(', join(',',@_), ')'.
	"\t(",$w->cget('-selectmode').
	',',$w->cget('-selectunit').")\n" if $DEBUG;

    if (0)
      {
	$@ = '';
	%@ = (); 		# Workaround to prevent spurious loss of $@
	eval { $w->$method(@_) };
	print STDERR "Error Gostate: '$state': ", $@ if $@;
	return undef;
      }

    $w->$method(@_);
    return undef
  }

##
## ChgState is a fancy case statement
##

sub ChgState
  {
    my $w   = shift;
    my $map = pop;
    print STDERR 'ChgState(', join(',',@_,'['), join(',',@$map,),'])  ' if $DEBUG;
    my $state = $w->GetState;

    my ($match, $to);
    while (@$map)
      {
        $match = shift @$map;
        $to    = shift @$map;
        if ($match eq $state)
          {
	    print STDERR "$state --> $to \n" if $DEBUG;
	    $w->GoState($to, @_);
	    return;
	  }
      }
    print STDERR "*no* chg for $state\n" if $DEBUG;
  }


#----------------------------------------------------------------------
#		   SELECTION ROUTINES
#----------------------------------------------------------------------

#proc tixGrid:SelectSingle {w ent} {
#    $w selection set [lindex $ent 0] [lindex $ent 1]
#    tixGrid:CallBrowseCmd $w $ent
#}

sub SelectSingle
  {
    my ($w, $n1, $n2) = @_;
    $w->selection('set', $n1, $n2);
    $w->Callback('-browsecmd' => $n1, $n2);
  }

#----------------------------------------------------------------------
#	SINGLE SELECTION
#----------------------------------------------------------------------

sub GoState_0
  {
    my ($w) = @_;
    my $list = $w->privateData()->{list};
    return unless defined $list;

    foreach my $cmd (@$list)
      {
        # XXX should do more something like $w->Callback'('__pending_cmds__');
	eval $cmd;		# XXX why in tcl in global context (binding?)
      }
    undef(@$list); 		# XXX should really delete? Maybe on needed in TCL
  }

# XXXX how to translate global context
#      what does unset
#proc tixGrid:GoState-0 {w} {
#    set list $w:_list
#    global $list
#
#    if [info exists $list] {
#	foreach cmd [set $list] {
#	    uplevel #0 $cmd
#	}
#	if [info exists $list] {
#	    unset $list
#	}
#    }
#}

sub GoState_1
   {
     my ($w, $x, $y) = @_;

     my @ent = $w->mynearest($x,$y);
     if (@ent)
       {
         $w->SetAnchor(@ent);
       }
     $w->CheckEdit;
     $w->selection('clear', 0, 0, 'max', 'max');

     if ($w->cget('-selectmode') ne 'single')
       {
         $w->SelectSingle(@ent);
       }
     $w->GoState(2);
   }

sub GoState_2
  {
    my ($w) = @_;
  }

sub GoState_3
   {
     my ($w, $x, $y) = @_;

     my @ent = $w->mynearest($x,$y);
     if (@ent)
       {
         $w->SelectSingle(@ent);
       }
     $w->GoState(0);

   }

sub GoState_4
  {
    my ($w, $x, $y) = @_;

    my (@ent) = $w->mynearest($x,$y);
    my $mode = $w->cget('-selectmode');

    if ($mode eq 'single')
      {
	$w->SetAnchor(@ent);
      }
    elsif ($mode eq 'browse')
      {
	$w->SetAnchor(@ent);
	$w->selection('clear', 0, 0, 'max', 'max');
	$w->SelectSingle(@ent);
      }
    elsif ($mode eq 'multiple' ||
	   $mode eq 'extended')
      {
	my (@anchor) = $w->anchor('get');
	$w->selection('adjust', @anchor[0,1], @ent[0,1]);
      }
  }

sub GoState_5
   {
     my ($w, $x, $y) = @_;

     my @ent = $w->mynearest($x,$y);
     if (@ent)
       {
         $w->SelectSingle(@ent);
         $w->SetEdit(@ent);
       }
     $w->GoState(0);

   }

##############################################
# BUG xxx
#	return scalar instead of errors

sub mynearest   { shift->split_s2a('nearest', @_); }
sub myanchorGet { shift->split_s2a('anchor', 'get', @_); }

sub split_s2a
  {
    my $w = shift;
    my $method = shift;
    my @ent = $w->$method(@_);
    if (@ent == 1)
      {
my $tmp = $ent[0];
        @ent = split(/ /, $ent[0]) if @ent == 1;
print STDERR join('|',"$method splitted '$tmp' =>",@ent,"\n") if $DEBUG;
      }
    else
      {
#print STDERR join("|","$method splitted is okay :",@ent,"\n") if $DEBUG;
      }
    return @ent;
  }

##############################################


sub GoState_s5
  {
    shift->StartScan();
  }


sub GoState_s6
  {
    shift->DoScan();
  }


sub GoState_s7
  {
    my ($w, $x, $y) = @_;

    my @ent = $w->mynearest($x, $y);
    if (@ent)
      {
        $w->selection('clear');
	$w->selection('set', @ent);
	$w->Callback('-command' => @ent);
      }
    $w->GoState('s0');
  }


sub GoState_s8
  {
    my ($w, $key) = @_;

    ## BUGS ....
    ## - anchor is bad, only bbox, exists8
    ## - looks like anchor is 1-dim: set anchor 0
    ## - method see unknown  (even when defined with Tk::Method)

    my (@anchor) = $w->info('anchor');
    if (@anchor)
      {
        @anchor = ();
      }
    else
      {
        @anchor = $w->info($key, @anchor);
      }

    $w->anchor('set', @anchor);
    $w->see(@anchor);

    $w->GoState('s0');
  }

#proc tixGrid:GoState-s8 {w key} {
#    set anchor [$w info anchor]
#
#    if {$anchor == ""} {
#	set anchor 0
#    } else {
#	set anchor [$w info $key $anchor]
#    }
#
#    $w anchor set $anchor
#    $w see $anchor
#    tixGrid:GoState s0 $w
#}


sub GoState_s9
  {
    my ($w, $key) = @_;

#print STDERR "GoState_s9 is not implemented\n";

    my (@anchor) = $w->info('anchor');
    unless (@anchor)
      {
        @anchor = ();
        $w->anchor('set', @anchor);
        $w->see(@anchor);
      }

    unless ($w->info('anchor'))
      {
        # ! may not have any elements
        #
        $w->Callback('-command' => $w->info('anchor'));
        $w->selection('clear');
        $w->selection('set', @anchor);
      }

      $w->GoState('s0');
  }


sub GoState_s10
  {
    my ($w, $key) = @_;

    my (@anchor) = $w->info('anchor');
    if (@anchor)
      {
        @anchor = ();
        $w->anchor('set', @anchor);
        $w->see(@anchor);
      }

    unless ($w->info('anchor'))
      {
        # ! may not have any elements
        #
        $w->Callback('-browsecmd' => $w->info('anchor'));
        $w->selection('clear');
        $w->selection('set', @anchor);
      }

    $w->GoState('s0');
  }


#----------------------------------------------------------------------
#	BROWSE SELECTION
#----------------------------------------------------------------------

sub GoState_b0
  {
    my ($w) = @_;
  }

sub GoState_b1
  {
    my ($w, $x, $y) = @_;

    my (@ent) = $w->mynearest($x, $y);
    if (@ent)
      {
	$w->anchor('set', @ent);
	$w->selection('clear');
	$w->selection('set', @ent);
	$w->Callback('-browsecmd' => @ent);
      }

    $w->GoState('b2');
  }

sub GoState_b2
  {
    my ($w) = @_;
  }

sub GoState_b3
  {
    my ($w) = @_;

    my (@ent) = $w->info('anchor');
    if (@ent)
      {
	$w->selection('clear');
	$w->selection('set', @ent);
	$w->selection('set', @ent);
	$w->Callback('-browsecmd' => @ent);
      }

    $w->GoState('b0');
  }


sub GoState_b4
  {
    my ($w, $x, $y) = @_;

    my (@ent) = $w->mynearest($x, $y);
    if (@ent)
      {
	$w->anchor('set', @ent);
	$w->selection('clear');
	$w->selection('set', @ent);
	$w->Callback('-browsecmd' => @ent);
      }
  }


sub GoState_b5 { shift->StartScan(); }


sub GoState_b6 { shift->DoScan(); }


sub GoState_b7
  {
     my ($w, $x, $y) = @_;

     my (@ent) = $w->mynearest($x, $y);
     if (@ent)
       {
         $w->selection('clear');
	 $w->selection('set', @ent);
         $w->Callback('-command' => @ent);
       }
     $w->GoState('b0');
  }


sub GoState_b8
  {
    my ($w, $key) = @_;

    my (@anchor) = $w->info('anchor');
    if (@anchor)
      {
	@anchor = $w->info('key', @anchor);
      }
    else
      {
        @anchor = (0,0);   # ?????
      }

    $w->anchor('set', @anchor);
    $w->selection('clear');
    $w->selection('set', @anchor);
    $w->see(@anchor);

    $w->Callback('-browsecmd' => @anchor);
    $w->GoState('b0');
  }


sub GoState_b9
  {
    my ($w) = @_;

    my (@anchor) =  $w->info('anchor');
    unless (@anchor)
      {
	@anchor = (0,0);
        $w->anchor('set', @anchor);
	$w->see(@anchor);
      }

    if ($w->info('anchor'))
      {
	# ! may not have any elements
	#
	$w->Callback('-command' => $w->info('anchor'));
	$w->selection('clear');
	$w->selection('set', @anchor);
      }

    $w->GoState('b0');
  }


sub GoState_b10
  {
    my ($w) = @_;

    my (@anchor) =  $w->info('anchor');
    unless (@anchor)
      {
	@anchor = (0,0);
        $w->anchor('set', @anchor);
	$w->see(@anchor);
      }

    if ($w->info('anchor'))
      {
	# ! may not have any elements
	#
	$w->Callback('-browsecmd' => $w->info('anchor'));
	$w->selection('clear');
	$w->selection('set', @anchor);
      }

    $w->GoState('b0');
  }

#----------------------------------------------------------------------
#	MULTIPLE SELECTION
#----------------------------------------------------------------------


sub GoState_m0
  {
    my ($w) = @_;
  }

sub GoState_m1
  {
    my ($w, $x, $y) = @_;

    my (@ent) = $w->mynearest($x,$y);
    if (@ent)
      {
	$w->anchor('set', @ent);
	$w->selection('clear');
	$w->selection('set', @ent);
	$w->Callback('-browsecmd' => @ent);
      }

    $w->GoState('m2');
  }

sub GoState_m2
  {
    my ($w) = @_;
  }

sub GoState_m3
  {
    my ($w) = @_;

    my (@ent) = $w->info('anchor');
    if (@ent)
      {
	$w->Callback('-browsecmd' => @ent);
      }

    $w->GoState('m0');
  }


sub GoState_m4
  {
    my ($w, $x, $y) = @_;

    my (@from) = $w->info('anchor');
    my (@to)   = $w->mynearest($x, $y);
    if (@to)
      {
	$w->selection('clear');
	$w->selection('set', @from, @to);
	$w->Callback('-browsecmd' => @to);
      }
    $w->GoState('m5');
  }

sub GoState_m5
  {
    my ($w) = @_;
  }

sub GoState_m6
  {
    my ($w, $x, $y) = @_;

    my (@ent)   = $w->mynearest($x, $y);
    if (@ent)
      {
	$w->Callback('-browsecmd' => @ent);
      }
    $w->GoState('m0');
  }

sub GoState_m7
  {
    my ($w, $x, $y) = @_;

    my (@from) = $w->info('anchor');
    my (@to)   = $w->mynearest($x, $y);
    unless (@from)
      {
	@from = @to;
	$w->anchor('set', @from);
      }
    if (@to)
      {
	$w->selection('clear');
	$w->selection('set', @from, @to);
	$w->Callback('-browsecmd' => @to);
      }
    $w->GoState('m5');
  }


sub GoState_m8 { shift->StartScan() }


sub GoState_m9 { shift->DoScan() }


sub GoState_xm7
  {
    my ($w, $x, $y) = @_;

    my (@ent)   = $w->mynearest($x, $y);
    if (@ent)
      {
	$w->selection('clear');
	$w->selection('set', @ent);
	$w->Callback('-browsecmd' => @ent);
      }
    $w->GoState('m0');
  }

#----------------------------------------------------------------------
#	EXTENDED SELECTION
#----------------------------------------------------------------------

sub GoState_e0
  {
    my ($w) = @_;
  }

sub GoState_e1
  {
    my ($w, $x, $y) = @_;
    my (@ent) = $w->mynearest($x, $y);
    if (@ent)
      {
	$w->anchor('set', @ent);
	$w->selection('clear');
	$w->selection('set', @ent);
	$w->Callback('-browsecmd' => @ent);
      }
    $w->GoState('e2');
  }


sub GoState_e2
  {
    my ($w) = @_;
  }

sub GoState_e3
  {
    my ($w) = @_;

    my (@ent) = $w->info('anchor');
    if (@ent)
      {
        $w->Callback('-browsecmd' => @ent);
      }
    $w->GoState('e0');
  }

sub GoState_e4
  {
    my ($w, $x, $y) = @_;

    my (@from) = $w->info('anchor');
    my (@to)   = $w->mynearest($x, $y);
    if (@to)
      {
        $w->selection('clear');
        $w->selection('set', @from, @to);
        $w->Callback('-browsecmd' => @to);
      }
    $w->GoState('e5');
  }

sub GoState_e5
  {
    my ($w) = @_;
  }

sub GoState_e6
  {
    my ($w, $x, $y) = @_;

    my (@ent)   = $w->mynearest($x, $y);
    if (@ent)
      {
        $w->Callback('-browsecmd' => @ent);
      }
    $w->GoState('e0');
  }

sub GoState_e7
  {
    my ($w, $x, $y) = @_;

    my (@from) = $w->info('anchor');
    my (@to)   = $w->mynearest($x, $y);
    unless (@from)
      {
        @from = @to;
        $w->anchor('set', @from);
      }
    if (@to)
      {
        $w->selection('clear');
        $w->selection('set', @from, @to);
        $w->Callback('-browsecmd' => @to);
      }
    $w->GoState('e5');
  }

sub GoState_e8 { shift->StartScan(); }

sub GoState_e9 { shift->DoScan(); }

sub GoState_e10
  {
    my ($w, $x, $y) = @_;

    my (@ent)   = $w->mynearest($x, $y);
    if (@ent)
      {
	if ($w->info('anchor'))
	  {
	    $w->anchor('set', @ent);
	  }
	if ($w->selection('includes', @ent))
	  {
	    $w->selection('clear', @ent);
	  }
	else
	  {
	    $w->selection('set', @ent);
	  }
	$w->Callback('-browsecmd' => @ent);
      }
    $w->GoState('e2');
  }

sub GoState_xe7
  {
    my ($w, $x, $y) = @_;

    my (@ent) = $w->mynearest($x, $y);
    if (@ent)
      {
        $w->selection('clear');
        $w->selection('set', @ent);
        $w->Callback('-command' => @ent);
      }
    $w->GoState('e0');
  }


#----------------------------------------------------------------------
#	HODGE PODGE
#----------------------------------------------------------------------

sub GoState_12
  {
    my ($w, $x, $y) = @_;

    $w->CancelRepeat;		# xxx  will not work
    $w->GoState(5, $x, $y);
  }
#proc tixGrid:GoState-12 {w x y} {
#    tkCancelRepeat
#    tixGrid:GoState 5 $w $x $y
#}

sub GoState_13
  {
    # FIX:  a) $ent or @ent, b) 13 is never called!!? same in Grid.tcl
    my ($w, @ent, @oldEnt) = @_;

    my $data = $w->MainWindow->privateData('Tix');
    $data->{indicator} = \@ent;
    $data->{oldEntry}  = \@oldEnt;
    $w->IndicatorCmd('<Arm>', @ent);
  }
#    set tkPriv(tix,oldEnt)    $oldEnt
#    tixGrid:IndicatorCmd $w <Arm> $ent
#}

sub GoState_14
  {
    my ($w, $x, $y) = @_;

    my $data = $w->MainWindow->privateData('Tix');
    if ($w->InsideArmedIndicator($x, $y))
      {
	$w->anchor('set', @{ $data->{indicator} });
	$w->selection('clear');
	$w->selection('set', @{ $data->{indicator} });
	$w->IndicatorCmd('<Activate>', @{ $data->{indicator} });
      }
    else
      {
	$w->IndicatorCmd('<Disarm>', @{ $data->{indicator} });
      }
    delete($data->{indicator});
    $w->GoState(0);
  }

sub GoState_16
  {
    my ($w, @ent) = @_;

    return unless (@ent);
    if ($w->cget('-selectmode') ne 'single')
      {
	$w->Select(@ent);
	$w->Browse(@ent);
      }
  }

sub GoState_18
  {
    my ($w) = @_;

    $w->CancelRepeat;	## xxx
    $w->GoState(6, $Tk::x, $Tk::y);
  }

sub GoState_20
  {
    my ($w, $x, $y) = @_;

    my $data = $w->MainWindow->privateData('Tix');
    if ($w->InsideArmedIndicator($x, $y))
      {
	$w->IndicatorCmd('<Arm>', $data->{'indicator'});
      }
    else
      {
	$w->GoState(21, $x, $y);
      }
  }

sub GoState_21
  {
    my ($w, $x, $y) = @_;

    my $data = $w->MainWindow->privateData('Tix');
    unless ($w->InsideArmedIndicator($x, $y))
      {
	$w->IndicatorCmd('<Disarm>', $data->{'indicator'});
      }
    else
      {
	$w->GoState(20, $x, $y);
      }
  }

sub GoState_22
  {
    my ($w) = @_;
    my $data = $w->MainWindow->privateData('Tix');
    if (@{ $data->{oldEntry} })
      {
	$w->anchor('set', @{ $data->{oldEntry} });
      }
    else
      {
	$w->anchor('clear');
      }
    $w->GoState(0);
  }


#----------------------------------------------------------------------
#			callback actions
#----------------------------------------------------------------------

sub SetAnchor
  {
    my ($w, @ent) = @_;

    if (@ent)
      {
	$w->anchor('set', @ent);
#	$w->see(@ent);
      }
  }

# xxx check @ent of @$ent
sub Select
  {
    my ($w, @ent) = @_;
    $w->selection('clear');
    $w->selection('set', @ent)
  }

# xxx check new After handling
sub StartScan
  {
    my ($w) = @_;
    $Tk::afterId = $w->after(50, [AutoScan, $w]);
  }

sub DoScan
  {
    my ($w) = @_;
    my $x = $Tk::x;
    my $y = $Tk::y;
    my $X = $Tk::X;
    my $Y = $Tk::Y;

    my $out = 0;
    if ($y >= $w->height)
      {
	$w->yview('scroll', 1, 'units');
	$out = 1;
      }
    if ($y < 0)
      {
	$w->yview('scroll', -1, 'units');
	$out = 1;
      }
    if ($x >= $w->width)
      {
	$w->xview('scroll', 2, 'units');
	$out = 1;
      }
    if ($x < 0)
      {
	$w->xview('scroll', -2, 'units');
	$out = 1;
      }
    if ($out)
      {
	$Tk::afterId = $w->after(50, ['AutoScan', $w]);
      }
  }


#proc tixGrid:CallBrowseCmd {w ent} {
#    return
#
#    set browsecmd [$w cget -browsecmd]
#    if {$browsecmd != ""} {
#	set bind(specs) {%V}
#	set bind(%V)    $ent
#
#	tixEvalCmdBinding $w $browsecmd bind $ent
#    }
#}

#proc tixGrid:CallCommand {w ent} {
#    set command [$w cget -command]
#    if {$command != ""} {
#	set bind(specs) {%V}
#	set bind(%V)    $ent
#
#	tixEvalCmdBinding $w $command bind $ent
#    }
#}

# tixGrid:EditCell --
#
#	This command is called when "$w edit set $x $y" is called. It causes
#	an SetEdit call when the grid's state is 0.
#

sub EditCell
  {
    my ($w, $x, $y) = @_;
    my $list = $w->privateData()->{'list'};
    if ($w->GetState == 0)
      {
	$w->SetEdit($x, $y);	# xxx really correct ? once 2, once 4 args?
      }
    else
      {
	push(@$list, [ $w, 'SetEdit', $x, $y]);
      }
  }
#proc tixGrid:EditCell {w x y} {
#    set list $w:_list
#    global $list
#
#    case [tixGrid:GetState $w] {
#	{0} {
#	    tixGrid:SetEdit $w [list $x $y]
#       	}
#	default {
#	    lappend $list [list tixGrid:SetEdit $w [list $x $y]]
#	}
#    }
#}


# tixGrid:EditApply --
#
#	This command is called when "$w edit apply $x $y" is called. It causes
#	an CheckEdit call when the grid's state is 0.
#

sub EditApply
  {
    my ($w) = @_;
    my $list = $w->privateData()->{'list'};
    if ($w->GetState eq 0)
      {
	$w->CheckEdit;	# xxx really correct ? once 2, once 4 args?
      }
    else
      {
	push(@$list, $w->CheckEdit);
      }
  }
#proc tixGrid:EditApply {w} {
#    set list $w:_list
#    global $list
#
#    case [tixGrid:GetState $w] {
#	{0} {
#	    tixGrid:CheckEdit $w
#       	}
#	default {
#	    lappend $list [list tixGrid:CheckEdit $w]
#	}
#    }
#}

# tixGrid:CheckEdit --
#
#	This procedure is called when the user sets the focus on a cell.
#	If another cell is being edited, apply the changes of that cell.
#

sub CheckEdit
  {
    my ($w) = @_;
    my $edit = $w->privateData->{editentry};
    if (Tk::Exists($edit))
      {
        # If it -command is not empty, it is being used for another cell.
        # Invoke it so that the other cell can be updated.
        #
        if (defined  $edit->cget('-command'))
          {
            $edit->invoke;	# xxx no args??
          }
      }
  }

sub SetFocus
  {
    my ($w) = @_;
    if ($w->cget('-takefocus'))
      {
$w->focus;
#	# xxx translation of if ![string match $w.* [focus -displayof $w]] {
#	my $hasfocus = $w->focus(-displayof => $w)->pathname;
#	my $pathname = $w->pathname;
#        if ($hasfocus =~ /\Q$pathname\E.*/)
#	  {
#	    $w->focus
#	  }
      }
  }


# tixGrid:SetEdit --
#
#	Puts a floatentry on top of an editable entry.
#

sub SetEdit
  {
    my ($w, $px, $py) = @_;

    $w->CheckEdit;

    my $efc = $w->cget('-editnotifycmd');
    return unless ( defined($efc) && length($efc) );

    unless ($w->Callback('-editnotifycmd' => $px, $py))
      {
	print STDERR "editnotifycmd not defined or returned false\n";
        return;
      }

    my $oldvalue;
    if ($w->info('exists', $px, $py))
      {
	# if entry doesn't support -text option. Can't edit it.
	#
	# If the application wants to force editing of an entry, it could
	# delete or replace the entry in the editnotifyCmd procedure.
	#
	Tk::catch { $oldvalue = $w->entrycget($px, $py, '-text'); };
        if ($@)
          {
	    return;
	  }
      }
    else
      {
	$oldvalue = '';
      }

    my @bbox = $w->info('bbox', $px, $py);

    my $edit = $w->privateData()->{__EDIT__};
    unless (Tk::Exists($edit))
      {
        require Tk::FloatEntry;
        $edit = $w->FloatEntry();
	$w->privateData()->{__EDIT__} = $edit;
      }
    $edit->configure(-command=>[\&DoneEdit, $w,  $px, $py]);
    $edit->post(@bbox);
    $edit->configure(-value=>$oldvalue);
}


sub DoneEdit
  {
    my ($w, $x, $y, @args) = @_;

    my $edit = $w->privateData()->{__EDIT__};
    $edit->configure(-command=>undef);
    $edit->unpost;

    # FIX xxx
    # set value [tixEvent value]
    my $value = $edit->get;
    if ($w->info('exists', $x, $y))
      {
	Tk::catch { $w->entryconfigure($x, $y, -text=>$value) };
        if ($@)
	  {
	    return
	  }
      }
    elsif ( length($value) )
      {
	# This needs to be catch'ed because the default itemtype may
	# not support the -text option
	#
	Tk::catch { $w->set($x,$y,-text $value); };
        if ($@)
          {
	    return;
          }
      }
    else
      {
	return;
      }
    $w->Callback('-editdonecmd' => $x, $y);
  }

1;
__END__

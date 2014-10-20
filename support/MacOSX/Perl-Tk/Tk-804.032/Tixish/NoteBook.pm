package Tk::NoteBook;
#
# Implementation of NoteBook widget.
# Derived from NoteBook.tcl in Tix 4.0

# Contributed by Rajappa Iyer <rsi@earthling.net>
# Hacked by Nick for 'menu' traversal.
# Restructured by Nick

use vars qw($VERSION);

#$VERSION = sprintf '4.%03d', q$Revision: #9 $ =~ /\D(\d+)\s*$/;
$VERSION = '4.011';
require Tk::NBFrame;

use base  qw(Tk::Derived Tk::NBFrame);
Tk::Widget->Construct('NoteBook');
use strict;

use Tk qw(Ev);

use Carp;
require Tk::Frame;

sub TraverseToNoteBook;

sub ClassInit
{
 my ($class,$mw) = @_;
 # class binding does not work right due to extra level of
 # widget hierachy
 $mw->bind($class,'<ButtonPress-1>', ['MouseDown',Ev('x'),Ev('y')]);
 $mw->bind($class,'<ButtonRelease-1>', ['MouseUp',Ev('x'),Ev('y')]);

 $mw->bind($class,'<B1-Motion>', ['MouseDown',Ev('x'),Ev('y')]);
 $mw->bind($class,'<Left>', ['FocusNext','prev']);
 $mw->bind($class,'<Right>', ['FocusNext','next']);

 $mw->bind($class,'<Return>', 'SetFocusByKey');
 $mw->bind($class,'<space>', 'SetFocusByKey');
 return $class;
}

sub raised
{
 return shift->{'topchild'};
}

sub Populate
{
 my ($w, $args) = @_;

 $w->SUPER::Populate($args);
 $w->{'pad-x1'} = undef;
 $w->{'pad-x2'} = undef;
 $w->{'pad-y1'} = undef;
 $w->{'pad-y2'} = undef;

 $w->{'windows'} = [];
 $w->{'nWindows'} = 0;
 $w->{'minH'} = 1;
 $w->{'minW'} = 1;

 $w->{'counter'} = 0;
 $w->{'resize'} = 0;

 $w->ConfigSpecs(-ipadx => ['PASSIVE', 'ipadX', 'Pad', 0],
		 -ipady => ['PASSIVE', 'ipadY', 'Pad', 0],
		 -takefocus => ['SELF', 'takeFocus', 'TakeFocus', 0],
		 -dynamicgeometry => ['PASSIVE', 'dynamicGeometry', 'DynamicGeometry', 0]);

 # SetBindings
 $w->bind('<Configure>','MasterGeomProc');

 $args->{-slave} = 1;
 $args->{-takefocus} = 1;
 $args->{-relief} = 'raised';

 $w->QueueResize;
}


#---------------------------
# Public methods
#---------------------------

sub page_widget
{
 my $w = shift;
 $w->{'_pages_'} = {} unless exists $w->{'_pages_'};
 my $h = $w->{'_pages_'};
 if (@_)
  {
   my $name = shift;
   if (@_)
    {
     my $cw = shift;
     if (defined $cw)
      {
       $h->{$name} = $cw;
      }
     else
      {
       return delete $h->{$name};
      }
    }
   return $h->{$name};
  }
 else
  {
   return (values %$h);
  }
}

sub add
{
 my ($w, $child, %args) = @_;

 croak("$child already exists") if defined $w->page_widget($child);

 my $f = Tk::Frame->new($w,Name => $child,-relief => 'raised');

 my $ccmd = delete $args{-createcmd};
 my $rcmd = delete $args{-raisecmd};
 $f->{-createcmd} = Tk::Callback->new($ccmd) if (defined $ccmd);
 $f->{-raisecmd} = Tk::Callback->new($rcmd) if (defined $rcmd);

 # manage our geometry
 $w->ManageGeometry($f);
 # create default bindings
 $f->bind('<Configure>',[$w,'ClientGeomProc','-configure', $f]);
 $f->bind('<Destroy>',  [$w,'delete',$child,1]);
 $w->page_widget($child,$f);
 $w->{'nWindows'}++;
 push(@{$w->{'windows'}}, $child);
 $w->SUPER::add($child,%args);
 return $f;
}

sub raise
{
 my ($w, $child) = @_;
 return unless defined $child;
 if ($w->pagecget($child, -state) eq 'normal')
  {
   $w->activate($child);
   $w->focus($child);
   my $childw = $w->page_widget($child);
   if ($childw)
    {
     if (defined $childw->{-createcmd})
      {
       $childw->{-createcmd}->Call($childw);
       delete $childw->{-createcmd};
      }
     # hide the original visible window
     my $oldtop = $w->{'topchild'};
     if (defined($oldtop) && ($oldtop ne $child))
      {
       $w->page_widget($oldtop)->UnmapWindow;
      }
     $w->{'topchild'} = $child;
     my $myW = $w->Width;
     my $myH = $w->Height;

     if (!defined $w->{'pad-x1'}) {
	 $w->InitTabSize;
     }

     my $cW = $myW - $w->{'pad-x1'} - $w->{'pad-x2'} - 2 * (defined $w->{-ipadx} ? $w->{-ipadx} : 0);
     my $cH = $myH - $w->{'pad-y1'} - $w->{'pad-y2'} - 2 * (defined $w->{-ipady} ? $w->{-ipady} : 0);
     my $cX = $w->{'pad-x1'} + (defined $w->{-ipadx} ? $w->{-ipadx} : 0);
     my $cY = $w->{'pad-y1'} + (defined $w->{-ipady} ? $w->{-ipady} : 0);

     if ($cW > 0 && $cH > 0)
      {
       $childw->MoveResizeWindow($cX, $cY, $cW, $cH);
       $childw->MapWindow;
       $childw->raise;
      }
     if ((not defined $oldtop) || ($oldtop ne $child))
      {
       if (defined $childw->{-raisecmd})
	{
	 $childw->{-raisecmd}->Call($childw);
	}
      }
    }
  }
}

sub pageconfigure
{
 my ($w, $child, %args) = @_;
 my $childw = $w->page_widget($child);
 if (defined $childw)
  {
   my $ccmd = delete $args{-createcmd};
   my $rcmd = delete $args{-raisecmd};
   $childw->{-createcmd} = Tk::Callback->new($ccmd) if (defined $ccmd);
   $childw->{-raisecmd} = Tk::Callback->new($rcmd) if (defined $rcmd);
   $w->SUPER::pageconfigure($child, %args) if (keys %args);
  }
}

sub pages {
    my ($w) = @_;
    return @{$w->{'windows'}};
}

sub pagecget
{
 my ($w, $child, $opt) = @_;
 my $childw = $w->page_widget($child);
 if (defined $childw)
  {
   return $childw->{-createcmd} if ($opt =~ /-createcmd/);
   return $childw->{-raisecmd} if ($opt =~ /-raisecmd/);
   return $w->SUPER::pagecget($child, $opt);
  }
 else
  {
   carp "page $child does not exist";
  }
}

sub delete
{
 my ($w, $child, $destroy) = @_;
 my $childw = $w->page_widget($child,undef);
 if (defined $childw)
  {
   $childw->bind('<Destroy>', undef);
   $childw->destroy;
   @{$w->{'windows'}} = grep($_ ne $child, @{$w->{'windows'}});
   $w->{'nWindows'}--;
   $w->SUPER::delete($child);
   # see if the child to be deleted was the top child
   if ((defined $w->{'topchild'}) && ($w->{'topchild'} eq $child))
    {
     delete $w->{'topchild'};
     if ( @{$w->{'windows'}})
      {
       $w->raise($w->{'windows'}[0]);
      }
    }
  }
 else
  {
   carp "page $child does not exist" unless $destroy;
  }
}

#---------------------------------------
# Private methods
#---------------------------------------

sub MouseDown {
    my ($w, $x, $y) = @_;
    my $name = $w->identify($x, $y);
    $w->focus($name);
    $w->{'down'} = $name;
}

sub MouseUp {
    my ($w, $x, $y) = @_;
    my $name = $w->identify($x, $y);
    if ((defined $name) && (defined $w->{'down'}) &&
	($name eq $w->{'down'}) &&
	($w->pagecget($name, -state) eq 'normal')) {
	$w->raise($name);
    } else {
	$w->focus($name);
    }
}

sub FocusNext {
    my ($w, $dir) = @_;
    my $name;

    if (not defined $w->info('focus')) {
	$name = $w->info('active');
	$w->focus($name);
    } else {
	$name = $w->info('focus' . $dir);
	$w->focus($name);
    }
}

sub SetFocusByKey {
    my ($w) = @_;

    my $name = $w->info('focus');
    if (defined $name) {
	if ($w->pagecget($name, -state) eq 'normal') {
	    $w->raise($name);
	    $w->activate($name);
	}
    }
}

sub NoteBookFind {
    my ($w, $char) = @_;

    my $page;
    foreach $page (@{$w->{'windows'}}) {
	my $i = $w->pagecget($page, -underline);
	my $c = substr($page, $i, 1);
	if ($char =~ /$c/) {
	    if ($w->pagecget($page, -state) ne 'disabled') {
		return $page;
	    }
	}
    }
    return undef;
}

# This is called by TraveseToMenu when an <Alt-Keypress> occurs
# See the code in Tk.pm
sub FindMenu {
    my ($w, $char) = @_;

    my $page;
    foreach $page (@{$w->{'windows'}}) {
	my $i = $w->pagecget($page, -underline);
	my $l = $w->pagecget($page, -label);
	next if (not defined $l);
	my $c = substr($l, $i, 1);
	if ($char =~ /$c/i) {
	    if ($w->pagecget($page, -state) ne 'disabled') {
		$w->raise($page);
		return $w;
	    }
	}
    }
    return undef;
}


sub MasterGeomProc
{
 my ($w) = @_;
 if (Tk::Exists($w))
  {
   $w->{'resize'} = 0 unless (defined $w->{'resize'});
   $w->QueueResize;
  }
}

sub SlaveGeometryRequest
{
 my $w = shift;
 if (Tk::Exists($w))
  {
   $w->QueueResize;
  }
}

sub LostSlave {
    my ($w, $s) = @_;
    $s->UnmapWindow;
}

sub ClientGeomProc
{
 my ($w, $flag, $client) = @_;
 $w->QueueResize if (Tk::Exists($w));
 if ($flag =~ /-lostslave/)
  {
   carp "Geometry Management Error: Another geometry manager has taken control of $client. This error is usually caused because a widget has been created in the wrong frame: it should have been created inside $client instead of $w";
  }
}

sub QueueResize
{
 my $w = shift;
 $w->afterIdle(['Resize', $w]) unless ($w->{'resize'}++);
}

sub Resize {

    my ($w) = @_;

    return unless Tk::Exists($w) && $w->{'nWindows'} && $w->{'resize'};

    $w->InitTabSize;

    $w->{'resize'} = 0;
    my $reqW = $w->{-width} || 0;
    my $reqH = $w->{-height} || 0;

    if ($reqW * $reqH == 0)
     {
	if ((not defined $w->cget('-dynamicgeometry')) ||
	    ($w->cget('-dynamicgeometry') == 0)) {
	    $reqW = 1;
	    $reqH = 1;

	    my $childw;
	    foreach $childw ($w->page_widget)
	     {
		my $cW = $childw->ReqWidth;
		my $cH = $childw->ReqHeight;
		$reqW = $cW if ($reqW < $cW);
		$reqH = $cH if ($reqH < $cH);
	    }
	} else {
	    if (defined $w->{'topchild'}) {
		my $topw = $w->page_widget($w->{'topchild'});
		$reqW = $topw->ReqWidth;
		$reqH = $topw->ReqHeight;
	    } else {
		$reqW = 1;
		$reqH = 1;
	    }
	}
	$reqW += $w->{'pad-x1'} + $w->{'pad-x2'} + 2 * (defined $w->{-ipadx} ? $w->{-ipadx} : 0);
	$reqH += $w->{'pad-y1'} + $w->{'pad-y2'} + 2 * (defined $w->{-ipady} ? $w->{-ipady} : 0);
	$reqW = ($reqW > $w->{'minW'}) ? $reqW : $w->{'minW'};
	$reqH = ($reqH > $w->{'minH'}) ? $reqH : $w->{'minH'};
    }
    if (($w->ReqWidth != $reqW) ||
	($w->ReqHeight != $reqH)) {
	$w->{'counter'} = 0 if (not defined $w->{'counter'});
	if ($w->{'counter'} < 50) {
	    $w->{'counter'}++;
	    $w->GeometryRequest($reqW, $reqH);
	    $w->afterIdle([$w,'Resize']);
	    $w->{'resize'} = 1;
	    return;
	}
    }
    $w->{'counter'} = 0;
    $w->raise($w->{'topchild'} || ${$w->{'windows'}}[0]);
    $w->{'resize'} = 0;
}

sub InitTabSize {
    my ($w) = @_;
    my ($tW, $tH) = $w->geometryinfo;
    $w->{'pad-x1'} = 2;
    $w->{'pad-x2'} = 2;
    $w->{'pad-y1'} = $tH + (defined $w->{'-ipadx'} ? $w->{'-ipadx'} : 0) + 1;
    $w->{'pad-y2'} = 2;
    $w->{'minW'} = $tW;
    $w->{'minH'} = $tH;
}

sub BalloonInfo
{
 my ($notebook,$balloon,$X,$Y,@opt) = @_;
 my $page = $notebook->identify($X-$notebook->rootx,$Y-$notebook->rooty);
 foreach my $opt (@opt)
  {
   my $info = $balloon->GetOption($opt,$notebook);
   if ($opt =~ /^-(statusmsg|balloonmsg)$/ && UNIVERSAL::isa($info,'HASH'))
    {
     if (!defined $page)
      {
       $balloon->Deactivate;
       return;
      }
     $balloon->Subclient($page);
     if (exists $info->{$page})
      {
       return $info->{$page}
      }
     else
      {
       return '';
      }
    }
   return $info;
  }
}

1;

__END__


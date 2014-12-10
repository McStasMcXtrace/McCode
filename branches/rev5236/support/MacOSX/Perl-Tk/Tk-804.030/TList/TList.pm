package Tk::TList;

use vars qw($VERSION);
$VERSION = '4.006'; # $Id: //depot/Tkutf8/TList/TList.pm#6 $

use Tk qw(Ev $XS_VERSION);

use base  qw(Tk::Widget);

use strict;

Construct Tk::Widget 'TList';

bootstrap Tk::TList;

sub Tk_cmd { \&Tk::tlist }

Tk::Methods qw(insert index anchor delete dragsite dropsite entrycget
               entryconfigure info nearest see selection xview yview);

use Tk::Submethods ( 'delete' => [qw(all entry offsprings siblings)],
                     'info' => [qw(anchor dragsite dropsite selection)],
                     'selection' => [qw(clear get includes set)],
                     'anchor' => [qw(clear set)],
                     'dragsite' => [qw(clear set)],
                     'dropsite' => [qw(clear set)],
                   );

sub ClassInit
{
 my ($class,$mw) = @_;

 $mw->bind($class,'<ButtonPress-1>',[ 'Button1' ] );
 $mw->bind($class,'<Shift-ButtonPress-1>',[ 'ShiftButton1' ] );
 $mw->bind($class,'<Control-ButtonRelease-1>','Control_ButtonRelease_1');
 $mw->bind($class,'<ButtonRelease-1>','ButtonRelease_1');
 $mw->bind($class,'<B1-Motion>',[ 'Button1Motion' ] );
 $mw->bind($class,'<B1-Leave>',[ 'AutoScan' ] );

 $mw->bind($class,'<Double-ButtonPress-1>',['Double1']);

 $mw->bind($class,'<Control-B1-Motion>','Control_B1_Motion');
 $mw->bind($class,'<Control-ButtonPress-1>',['CtrlButton1']);
 $mw->bind($class,'<Control-Double-ButtonPress-1>',['CtrlButton1']);

 $mw->bind($class,'<B1-Enter>','B1_Enter');

 $mw->bind($class,'<Up>',  ['DirKey', 'up']);
 $mw->bind($class,'<Down>',['DirKey', 'down']);

 $mw->bind($class,'<Left>', ['DirKey', 'left']);
 $mw->bind($class,'<Right>',['DirKey', 'right']);

 $mw->bind($class,'<Prior>','Prior');
 $mw->bind($class,'<Next>','Next');

 $mw->bind($class,'<Return>', ['KeyboardActivate']);
 $mw->bind($class,'<space>',  ['KeyboardBrowse']);

 return $class;
}

sub Control_ButtonRelease_1
{
}


sub ButtonRelease_1
{
 my $w = shift;
 my $Ev = $w->XEvent;
 $w->CancelRepeat
 if($w->cget('-selectmode') ne 'dragdrop');
 $w->ButtonRelease1($Ev);
}


sub Control_B1_Motion
{
}


sub B1_Enter
{
 my $w = shift;
 my $Ev = $w->XEvent;
 $w->CancelRepeat
 if($w->cget('-selectmode') ne 'dragdrop');
}


sub Prior
{
shift->yview('scroll', -1, 'pages')
}


sub Next
{
shift->yview('scroll',  1, 'pages')
}


sub Button1
{
 my $w = shift;
 my $Ev = $w->XEvent;

 delete $w->{'shiftanchor'};

 $w->focus()
    if($w->cget('-takefocus'));

 my $mode = $w->cget('-selectmode');

 if ($mode eq 'dragdrop')
  {
   # $w->Send_WaitDrag($Ev->y);
   return;
  }

 my $ent = $w->GetNearest($Ev->x, $Ev->y);

 return unless defined $ent;

 my $browse = 0;

 if($mode eq 'single')
  {
   $w->anchor('set', $ent);
  }
 elsif($mode eq 'browse')
  {
   $w->anchor('set', $ent);
   $w->selection('clear' );
   $w->selection('set', $ent);
   $browse = 1;
  }
 elsif($mode eq 'multiple')
  {
   $w->selection('clear');
   $w->anchor('set', $ent);
   $w->selection('set', $ent);
   $browse = 1;
  }
 elsif($mode eq 'extended')
  {
   $w->anchor('set', $ent);
   $w->selection('clear');
   $w->selection('set', $ent);
   $browse = 1;
  }

 $w->Callback(-browsecmd => $ent) if ($browse);
}

sub ShiftButton1
{
 my $w = shift;
 my $Ev = $w->XEvent;

 my $to = $w->GetNearest($Ev->x,$Ev->y);

 delete $w->{'shiftanchor'};

 return unless defined $to;

 my $mode = $w->cget('-selectmode');

 if ($mode eq 'extended')
  {
   my $from = $w->info('anchor');
   if (defined $from)
    {
     $w->selection('clear');
     $w->selection('set', $from, $to);
    }
   else
    {
     $w->anchor('set', $to);
     $w->selection('clear');
     $w->selection('set', $to);
    }
  }
}

sub GetNearest
{
 my ($w,$x,$y) = @_;
 my $ent = $w->nearest($x,$y);
 if (defined $ent)
  {
   my $state = $w->entrycget($ent, '-state');
   return $ent if (!defined($state) || $state ne 'disabled');
  }
 return undef;
}

sub ButtonRelease1
{
 my ($w, $Ev) = @_;

 delete $w->{'shiftanchor'};

 my $mode = $w->cget('-selectmode');

 if($mode eq 'dragdrop')
  {
#   $w->Send_DoneDrag();
   return;
  }

 my ($x, $y) = ($Ev->x, $Ev->y);
 my $ent = $w->GetNearest($x,$y);

 return unless defined $ent;

 if($x < 0 || $y < 0 || $x > $w->width || $y > $w->height)
  {
   $w->selection('clear');

   return if($mode eq 'single' || $mode eq 'browse')

  }
 else
  {
   if($mode eq 'single' || $mode eq 'browse')
    {
     $w->anchor('set', $ent);
     $w->selection('clear');
     $w->selection('set', $ent);

    }
   elsif($mode eq 'multiple')
    {
     $w->selection('set', $ent);
    }
   elsif($mode eq 'extended')
    {
     $w->selection('set', $ent);
    }
  }

 $w->Callback(-browsecmd =>$ent);
}

sub Button1Motion
{
 my $w = shift;
 my $Ev = $w->XEvent;

 delete $w->{'shiftanchor'};

 my $mode = $w->cget('-selectmode');

 if ($mode eq 'dragdrop')
  {
#   $w->Send_StartDrag();
   return;
  }

 my $ent = $w->GetNearest($Ev->x,$Ev->y);

 return unless defined $ent;

 if($mode eq 'single')
  {
   $w->anchor('set', $ent);
  }
 elsif($mode eq 'multiple' || $mode eq 'extended')
  {
   my $from = $w->info('anchor');
   if (defined $from)
    {
     $w->selection('clear');
     $w->selection('set', $from, $ent);
    }
   else
    {
     $w->anchor('set', $ent);
     $w->selection('clear');
     $w->selection('set', $ent);
    }
  }

 if($mode ne 'single')
  {
   $w->Callback(-browsecmd =>$ent);
  }
}

sub Double1
{
 my $w = shift;
 my $Ev = $w->XEvent;

 delete $w->{'shiftanchor'};

 my $ent = $w->GetNearest($Ev->x,$Ev->y);

 return unless defined $ent;

 $w->anchor('set', $ent) unless defined($w->info('anchor'));

 $w->selection('set', $ent);
 $w->Callback(-command => $ent);
}

sub CtrlButton1
{
 my $w = shift;
 my $Ev = $w->XEvent;

 delete $w->{'shiftanchor'};

 my $ent = $w->GetNearest($Ev->x,$Ev->y);

 return unless defined $ent;

 my $mode = $w->cget('-selectmode');

 if($mode eq 'extended')
  {
   $w->anchor('set', $ent) unless defined( $w->info('anchor') );

   if($w->selection('includes', $ent))
    {
     $w->selection('clear', $ent);
    }
   else
    {
     $w->selection('set', $ent);
    }
   $w->Callback(-browsecmd =>$ent);
  }
}

sub DirKey
{
 my ($w,$dir) = @_;
 my $anchor = $w->info('anchor');

 my $new = (defined $anchor) ? $w->info($dir,$anchor) : 0;

 $w->anchorSet($new);
 $w->see($new);
}

sub KeyboardActivate
{
 my $w = shift;

 my $anchor = $w->info('anchor');

 return unless defined $anchor;

 if($w->cget('-selectmode'))
  {
   $w->selection('clear');
   $w->selection('set', $anchor);
  }
 $w->Callback(-command => $anchor);
}

sub KeyboardBrowse
{
 my $w = shift;

 my $anchor = $w->info('anchor');

 return unless defined $anchor;

 if($w->cget('-selectmode'))
  {
   $w->selection('clear');
   $w->selection('set', $anchor);
  }
 $w->Callback(-browsecmd =>$anchor);
}

sub AutoScan
{
 my $w = shift;

 return if($w->cget('-selectmode') eq 'dragdrop');

 my $Ev = $w->XEvent;
 my $y = $Ev->y;
 my $x = $Ev->x;

 if($y >= $w->height)
  {
   $w->yview('scroll', 1, 'units');
  }
 elsif($y < 0)
  {
   $w->yview('scroll', -1, 'units');
  }
 elsif($x >= $w->width)
  {
   $w->xview('scroll', 2, 'units');
  }
 elsif($x < 0)
  {
   $w->xview('scroll', -2, 'units');
  }
 else
  {
   return;
  }
 $w->RepeatId($w->after(50,[AutoScan => $w]));
 $w->Button1Motion;
}

1;


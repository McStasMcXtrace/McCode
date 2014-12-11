package Tk::HList;

use vars qw($VERSION);
$VERSION = '4.015'; # was: sprintf '4.%03d', q$Revision: #14 $ =~ /\D(\d+)\s*$/;

use Tk qw(Ev $XS_VERSION);

use base  qw(Tk::Widget);

Construct Tk::Widget 'HList';
sub Tk::Widget::ScrlHList { shift->Scrolled('HList'=>@_) }

bootstrap Tk::HList;

sub Tk_cmd { \&Tk::hlist }

sub CreateArgs
{
 my ($package,$parent,$args) = @_;
 my @result = $package->SUPER::CreateArgs($parent,$args);
 my $columns = delete $args->{-columns};
 push(@result, '-columns' => $columns) if (defined $columns);
 return @result;
}

Tk::Methods qw(add addchild anchor column
               delete dragsite dropsite entrycget
               entryconfigure geometryinfo indicator header hide item info
               nearest see select selection show xview yview);

use Tk::Submethods ( 'delete'    => [qw(all entry offsprings siblings)],
                     'header'    => [qw(configure cget create delete exists size)],
                     'indicator' => [qw(configure cget create delete exists size)],
                     'info'      => [qw(anchor bbox children data dragsite
                                     dropsite exists hidden item next parent prev
                                     selection)],
                     'item'      => [qw(configure cget create delete exists)],
                     'selection' => [qw(clear get includes set)],
                     'anchor'    => [qw(clear set)],
                     'column'    => [qw(width)],
                   );

# This is undocumented, but worked until 804.027:
sub hideEntry { shift->hide('entry', @_) }

sub ClassInit
{
 my ($class,$mw) = @_;

 $mw->bind($class,'<ButtonPress-1>',[ 'Button1' ] );
 $mw->bind($class,'<Shift-ButtonPress-1>',[ 'ShiftButton1' ] );
 $mw->bind($class,'<Control-ButtonRelease-1>','Control_ButtonRelease_1');
 $mw->bind($class,'<ButtonRelease-1>','ButtonRelease_1');
 $mw->bind($class,'<Double-ButtonRelease-1>','NoOp');
 $mw->bind($class,'<B1-Motion>',[ 'Button1Motion' ] );
 $mw->bind($class,'<B1-Leave>',[ 'AutoScan' ] );

 $mw->bind($class,'<Double-ButtonPress-1>',['Double1']);

 $mw->bind($class,'<Control-B1-Motion>','Control_B1_Motion');
 $mw->bind($class,'<Control-ButtonPress-1>',['CtrlButton1']);
 $mw->bind($class,'<Control-Double-ButtonPress-1>',['CtrlButton1']);

 $mw->bind($class,'<B1-Enter>','B1_Enter');

 $mw->bind($class,'<Up>',['UpDown', 'prev']);
 $mw->bind($class,'<Down>',['UpDown', 'next']);

 $mw->bind($class,'<Shift-Up>',['ShiftUpDown', 'prev']);
 $mw->bind($class,'<Shift-Down>',['ShiftUpDown', 'next']);

 $mw->bind($class,'<Left>', ['LeftRight', 'left']);
 $mw->bind($class,'<Right>',['LeftRight', 'right']);

 $mw->PriorNextBind($class);
 $mw->MouseWheelBind($class);

 $mw->bind($class,'<Return>', ['KeyboardActivate']);
 $mw->bind($class,'<space>',  ['KeyboardBrowse']);
 $mw->bind($class,'<Home>',   ['KeyboardHome']);
 $mw->bind($class,'<End>',    ['KeyboardEnd']);

 $mw->YMouseWheelBind($class);
 $mw->XMouseWheelBind($class);

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

sub Button1
{
 my $w = shift;
 my $Ev = $w->XEvent;

 delete $w->{'shiftanchor'};
 delete $w->{tixindicator};

 $w->focus() if($w->cget('-takefocus'));

 my $mode = $w->cget('-selectmode');

 if ($mode eq 'dragdrop')
  {
   # $w->Send_WaitDrag($Ev->y);
   return;
  }

 my $ent = $w->GetNearest($Ev->y, 1);

 if (!defined($ent) || !length($ent))
  {
    $w->selectionClear;
    $w->anchorClear;
    return;
  }

 my @info = $w->info('item',$Ev->x, $Ev->y);
 if (@info)
  {
   die 'Assert' unless $info[0] eq $ent;
  }
 else
  {
   @info = $ent;
  }

 if (defined($info[1]) && $info[1] eq 'indicator')
  {
   $w->{tixindicator} = $ent;
   $w->Callback(-indicatorcmd => $ent, '<Arm>');
  }
 else
  {
   my $browse = 0;

   if ($mode eq 'single')
    {
     $w->anchorSet($ent);
    }
   elsif ($mode eq 'browse')
    {
     $w->anchorSet($ent);
     $w->selectionClear;
     $w->selectionSet($ent);
     $browse = 1;
    }
   elsif ($mode eq 'multiple')
    {
     $w->selectionClear;
     $w->anchorSet($ent);
     $w->selectionSet($ent);
     $browse = 1;
    }
   elsif ($mode eq 'extended')
    {
     $w->anchorSet($ent);
     $w->selectionClear;
     $w->selectionSet($ent);
     $browse = 1;
    }

   if ($browse)
    {
     $w->Callback(-browsecmd => @info);
    }
  }
}

sub ShiftButton1
{
 my $w = shift;
 my $Ev = $w->XEvent;

 my $to = $w->GetNearest($Ev->y, 1);

 delete $w->{'shiftanchor'};
 delete $w->{tixindicator};

 return unless (defined($to) and length($to));

 my $mode = $w->cget('-selectmode');

 if($mode eq 'extended' or $mode eq 'multiple')
  {
   my $from = $w->info('anchor');
   if(defined $from)
    {
     $w->selectionClear;
     $w->selectionSet($from, $to);
    }
   else
    {
     $w->anchorSet($to);
     $w->selectionClear;
     $w->selectionSet($to);
    }
  }
}

sub GetNearest
{
 my ($w,$y,$undefafterend) = @_;
 my $ent = $w->nearest($y);
 if (defined $ent)
  {
   if ($undefafterend)
    {
     my $borderwidth = $w->cget('-borderwidth');
     my $highlightthickness = $w->cget('-highlightthickness');
     my $bottomy = ($w->infoBbox($ent))[3];
     $bottomy += $borderwidth + $highlightthickness;
     if ($w->header('exist', 0))
      {
       $bottomy += $w->header('height');
      }
     if ($y > $bottomy)
      {
       #print "$y > $bottomy\n";
       return undef;
      }
    }
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
 my $ent = $w->GetNearest($y, 1);

 if (!defined($ent) and $mode eq 'single')
  {
     my $ent = $w->info('selection');
     if (defined $ent)
      {
        $w->anchorSet($ent);
      }
  }
 return unless (defined($ent) and length($ent));

 if (exists $w->{tixindicator})
  {
   return unless delete($w->{tixindicator}) eq $ent;
   my @info = $w->info('item',$Ev->x, $Ev->y);
   if(defined($info[1]) && $info[1] eq 'indicator')
    {
     $w->Callback(-indicatorcmd => $ent, '<Activate>');
    }
   else
    {
     $w->Callback(-indicatorcmd => $ent, '<Disarm>');
    }
   return;
  }

  if($mode eq 'single' || $mode eq 'browse')
   {
    $w->anchorSet($ent);
    $w->selectionClear;
    $w->selectionSet($ent);

   }
  elsif($mode eq 'multiple')
   {
    $w->selectionSet($ent);
   }
  elsif($mode eq 'extended')
   {
    $w->selectionSet($ent);
   }

 $w->Callback(-browsecmd =>$ent);
}

sub Button1Motion
{
 my $w = shift;
 my $Ev = $w->XEvent;
 return unless defined $Ev;

 delete $w->{'shiftanchor'};

 my $mode = $w->cget('-selectmode');

 if ($mode eq 'dragdrop')
  {
#   $w->Send_StartDrag();
   return;
  }

 my $ent;
 if (defined $w->info('anchor'))
  {
   $ent = $w->GetNearest($Ev->y);
  }
 else
  {
   $ent = $w->GetNearest($Ev->y, 1);
  }
 return unless (defined($ent) and length($ent));

 if(exists $w->{tixindicator})
  {
   my $event_type = $w->{tixindicator} eq $ent ? '<Arm>' : '<Disarm>';
   $w->Callback(-indicatorcmd => $w->{tixindicator}, $event_type );
   return;
  }

 if ($mode eq 'single')
  {
   $w->anchorSet($ent);
  }
 elsif ($mode eq 'multiple' || $mode eq 'extended')
  {
   my $from = $w->info('anchor');
   if(defined $from)
    {
     $w->selectionClear;
     $w->selectionSet($from, $ent);
    }
   else
    {
     $w->anchorSet($ent);
     $w->selectionClear;
     $w->selectionSet($ent);
    }
  }

 if ($mode ne 'single')
  {
   $w->Callback(-browsecmd =>$ent);
  }
}

sub Double1
{
 my $w = shift;
 my $Ev = $w->XEvent;

 delete $w->{'shiftanchor'};

 my $ent = $w->GetNearest($Ev->y, 1);

 return unless (defined($ent) and length($ent));

 $w->anchorSet($ent)
	unless(defined $w->info('anchor'));

 $w->selectionSet($ent);

 $w->Callback(-command => $ent);
}

sub CtrlButton1
{
 my $w = shift;
 my $Ev = $w->XEvent;

 delete $w->{'shiftanchor'};

 my $ent = $w->GetNearest($Ev->y, 1);

 return unless (defined($ent) and length($ent));

 my $mode = $w->cget('-selectmode');

 if($mode eq 'extended')
  {
   $w->anchorSet($ent) unless( defined $w->info('anchor') );

   if($w->select('includes', $ent))
    {
     $w->select('clear', $ent);
    }
   else
    {
     $w->selectionSet($ent);
    }
   $w->Callback(-browsecmd =>$ent);
  }
}

sub UpDown
{
 my $w = shift;
 my $spec = shift;

 my $done = 0;
 my $anchor = $w->info('anchor');

 delete $w->{'shiftanchor'};

 unless( defined $anchor )
  {
   $anchor = ($w->info('children'))[0];

   return unless (defined($anchor) and length($anchor));

   if($w->entrycget($anchor, '-state') ne 'disabled')
    {
     # That's a good anchor
     $done = 1;
    }
   else
    {
     # We search for the first non-disabled entry (downward)
     $spec = 'next';
    }
  }

 my $ent = $anchor;

 # Find the prev/next non-disabled entry
 #
 while(!$done)
  {
   $ent = $w->info($spec, $ent);
   last unless( defined $ent );
   next if( $w->entrycget($ent, '-state') eq 'disabled' );
   next if( $w->info('hidden', $ent) );
   last;
  }

 unless( defined $ent )
  {
   $w->yview('scroll', $spec eq 'prev' ? -1 : 1, 'unit');
   return;
  }

 $w->anchorSet($ent);
 $w->see($ent);

 if($w->cget('-selectmode') ne 'single')
  {
   $w->selectionClear;
   $w->selection('set', $ent);
   $w->Callback(-browsecmd =>$ent);
  }
}

sub ShiftUpDown
{
 my $w = shift;
 my $spec = shift;

 my $mode = $w->cget('-selectmode');

 return $w->UpDown($spec)
   if($mode eq 'single' || $mode eq 'browse');

 my $anchor = $w->info('anchor');

 return $w->UpDown($spec) unless (defined($anchor) and length($anchor));

 my $done = 0;

 $w->{'shiftanchor'} = $anchor unless( $w->{'shiftanchor'} );

 my $ent = $w->{'shiftanchor'};

 while( !$done )
  {
   $ent = $w->info($spec, $ent);
   last unless( defined $ent );
   next if( $w->entrycget($ent, '-state') eq 'disabled' );
   next if( $w->info('hidden', $ent) );
   last;
  }

 unless( $ent )
  {
   $w->yview('scroll', $spec eq 'prev' ? -1 : 1, 'unit');
   return;
  }

 $w->selectionClear;
 $w->selection('set', $anchor, $ent);
 $w->see($ent);

 $w->{'shiftanchor'} = $ent;

 $w->Callback(-browsecmd =>$ent);
}

sub LeftRight
{
 my $w = shift;
 my $spec = shift;

 delete $w->{'shiftanchor'};

 my $anchor = $w->info('anchor');

 unless(defined $anchor)
  {
   $anchor = ($w->info('children'))[0]
  }
 unless(defined $anchor)
  {
   $anchor = '';
  }

 my $done = 0;
 my $ent = $anchor;

 while(!$done)
  {
   my $e = $ent;

   if($spec eq 'left')
    {
     $ent = $w->info('parent', $e);

     $ent = $w->info('prev', $e)
       unless(defined $ent && $w->entrycget($ent, '-state') ne 'disabled')
    }
   else
    {
     $ent = ($w->info('children', $e))[0];

     $ent = $w->info('next', $e)
       unless(defined $ent && $w->entrycget($ent, '-state') ne 'disabled')
    }

   last unless( defined $ent );
   last if($w->entrycget($ent, '-state') ne 'disabled');
  }

 unless( defined $ent )
  {
   $w->xview('scroll', $spec eq 'left' ? -1 : 1, 'unit');
   return;
  }

 $w->anchorSet($ent);
 $w->see($ent);

 if($w->cget('-selectmode') ne 'single')
  {
   $w->selectionClear;
   $w->selectionSet($ent);

   $w->Callback(-browsecmd =>$ent);
  }
}

sub KeyboardHome
{
 my $w = shift;
 $w->yview('moveto' => 0);
 $w->xview('moveto' => 0);
}

sub KeyboardEnd
{
 my $w = shift;
 $w->yview('moveto' => 1);
 $w->xview('moveto' => 0);
}

sub KeyboardActivate
{
 my $w = shift;

 my $anchor = $w->info('anchor');

 return unless (defined($anchor) and length($anchor));

 if($w->cget('-selectmode'))
  {
   $w->selectionClear;
   $w->selectionSet($anchor);
  }

 $w->Callback(-command => $anchor);
}

sub KeyboardBrowse
{
 my $w = shift;

 my $anchor = $w->info('anchor');

 return unless (defined($anchor) and length($anchor));

 if ($w->indicatorExists($anchor))
  {
   $w->Callback(-indicatorcmd => $anchor);
  }

 if($w->cget('-selectmode'))
  {
   $w->selectionClear;
   $w->selectionSet($anchor);
  }
 $w->Callback(-browsecmd =>$anchor);
}

sub AutoScan
{
 my ($w,$x,$y) = @_;

 return if ($w->cget('-selectmode') eq 'dragdrop');
 if (@_ < 3)
  {
   my $Ev = $w->XEvent;
   return unless defined $Ev;
   $y = $Ev->y;
   $x = $Ev->x;
  }

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
 $w->RepeatId($w->SUPER::after(50,[ AutoScan => $w, $x, $y ]));
 $w->Button1Motion;
}

sub children
{
 # Tix has core-tk window(s) which are not a widget(s)
 # the generic code returns these as an "undef"
 my $w = shift;
 my @info = grep(defined($_),$w->winfo('children'));
 @info;
}

sub BalloonInfo
{
 my ($listbox,$balloon,$X,$Y,@opt) = @_;
 my $e = $listbox->XEvent;
 return if !$e;
 my $path = $listbox->GetNearest($e->y, 1);
 $path = '' unless defined($path);
 foreach my $opt (@opt)
  {
   my $info = $balloon->GetOption($opt,$listbox);
   if ($opt =~ /^-(statusmsg|balloonmsg)$/
       && UNIVERSAL::isa($info,'HASH'))
    {
     $balloon->Subclient($path);
     if (defined $info->{$path})
      {
       return $info->{$path};
      }
     return '';
    }
   return $info if (defined $info);
  }
 return '';
}

1;

package Tk::DragDrop::XDNDDrop;
use strict;
use vars qw($VERSION);
$VERSION = '4.007'; # sprintf '4.%03d', q$Revision: #6 $ =~ /\D(\d+)\s*$/;
use base qw(Tk::DragDrop::Rect);

sub XDND_PROTOCOL_VERSION () { 4 }

Tk::DragDrop->Type('XDND');

sub NewDrag
{
 my ($class,$token) = @_;
 $token->{$class} = {};
}

sub new
{
 my ($class,$token,$id,@prop) = @_;
 my $ver = $token->InternAtom(shift(@prop));
 # warn "XDND version $ver ".join(' ',@prop)."\n";
 $ver = XDND_PROTOCOL_VERSION if $ver > XDND_PROTOCOL_VERSION;
 my $site = bless { id => $id, token => $token, ver => $ver, state => 0, accept => \@prop}, $class;
 my $w = $token->parent;
 $w->BindClientMessage('XdndStatus',[$site => 'XdndStatus']);
 $w->BindClientMessage('XdndFinished',[$site => 'XdndFinished']);
 return $site;
}

sub Drop
{
 my ($site,$token,$seln,$e) = @_;
 my $w   = $token->parent;
 my $data = pack('LLLLL',oct($w->id),0,$e->t,0,0);
 $w->SendClientMessage('XdndDrop',$site->{id},32,$data);
}

sub FindSite
{
 my ($class,$token,$X,$Y) = @_;
 my $id = $token->PointToWindow($X,$Y);
 while ($id)
  {
   my @prop;
   Tk::catch { @prop = $token->property('get','XdndAware', $id) };
   if (!$@ && shift(@prop) eq 'ATOM')
    {
     my $hash = $token->{$class};
     my $site = $hash->{$id};
     if (!defined $site)
      {
       $site = $class->new($token,$id,@prop);
       $hash->{$id} = $site;
      }
     return $site;
    }
   $id = $token->PointToWindow($X,$Y,$id)
  }
 return undef;
}

sub Enter
{
 my ($site,$token,$e) = @_;
 my $w   = $token->parent;
 $token->InstallHandlers('XdndSelection');
 my $seln = $token->cget('-selection');
 my @targets = grep(!/^(TARGETS|MULTIPLE|TIMESTAMP)$/,reverse($token->SelectionGet('-selection'=> 'XdndSelection','TARGETS')));
 # print join(' ',@targets),"\n";
 my $flags   = ($site->{ver} << 24);
 my @atarg   = map($token->InternAtom($_),@targets);
 my $ntarg   = @atarg;
 if ($ntarg > 3)
  {
   $flags |= 1;
   $w->property('set','XdndTypeList','ATOM',32,\@atarg);
   splice(@atarg,3);
  }
 else
  {
   splice(@atarg,$ntarg,(0 x 3 - $ntarg));
  }
 unshift(@atarg,oct($w->id),$flags);
 # print join(' ',map(sprintf("%08X",$_),@atarg)),"\n";
 my $data = pack('LLLLL',@atarg);
 $w->SendClientMessage('XdndEnter',$site->{id},32,$data);
}

sub Leave
{
 my ($site,$token,$e) = @_;
 my $w   = $token->parent;
 my $data = pack('LLLLL',oct($w->id), 0, 0, 0, 0);
 $w->SendClientMessage('XdndLeave',$site->{id},32,$data);
}

sub Motion
{
 my ($site,$token,$e) = @_;
 my $X = $e->X;
 my $Y = $e->Y;
 my $w   = $token->parent;
 my $action = $token->InternAtom($site->{'action'} || 'XdndActionCopy');
 my @atarg = (oct($w->id),0,($X << 16) | $Y, $e->t, $action);
 # print join(' ',map(sprintf("%08X",$_),@atarg)),"\n";
 my $data = pack('LLLLL',@atarg);
 $w->SendClientMessage('XdndPosition',$site->{id},32,$data);
}

sub XdndFinished
{
 my ($site) = @_;
 my $token = $site->{token};
 # printf "XdndFinished $site\n",
 $token->Done;
}

sub XdndStatus
{
 my ($site) = @_;
 my $token = $site->{token};
 my $w   = $token->parent;
 my $event = $w->XEvent;
 my ($tid,$flags,$xy,$wh,$action) = unpack('LLLLL',$event->A);
 $action = $w->GetAtomName($action) if $action;
 $site->{flags} = $flags;
 $site->{'X'}   = $xy >> 16;
 $site->{'Y'}   = $xy & 0xFFFF;
 $site->{'width'}  = $wh >> 16;
 $site->{'height'} = $wh & 0xFFFF;
 #printf "XdndStatus $site targ=%x flags=%08X x=%d y=%d w=%d h=%d a=%s\n",
 #        $tid,$flags,$xy >> 16, $xy & 0xFFFF, $wh >> 16, $wh & 0xFFFF,$action;
 if ($flags & 1)
  {
   $token->AcceptDrop;
  }
 else
  {
   $token->RejectDrop;
  }
}


1;
__END__

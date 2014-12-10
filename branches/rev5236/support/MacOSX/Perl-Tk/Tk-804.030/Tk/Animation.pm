package Tk::Animation;

use vars qw($VERSION);
$VERSION = '4.008'; # $Id: //depot/Tkutf8/Tk/Animation.pm#8 $

use Tk::Photo;
use base  qw(Tk::Photo);

Construct Tk::Widget 'Animation';

sub MainWindow
{
 return shift->{'_MainWIndow_'};
}

sub add_frame
{
 my $obj = shift;
 $obj->{'_frames_'} = [] unless exists $obj->{'_frames_'};
 push(@{$obj->{'_frames_'}},@_);
}

sub new
{
 my ($class,$widget,%args) = @_;
 my $obj = $class->SUPER::new($widget,%args);
 $obj->{'_MainWIndow_'} = $widget->MainWindow;
 if ($args{'-format'} eq 'gif')
  {
   my @images;
   local $@;
   while (1)
    {
     my $index = @images;
     $args{'-format'} = "gif -index $index";
     my $img;
     eval {local $SIG{'__DIE__'};  $img = $class->SUPER::new($widget,%args) };
     last if $@;
     push(@images,$img);
    }
   if (@images > 1)
    {
     $obj->add_frame(@images);
     $obj->{'_frame_index_'}  = 0;
    }
  }
 $obj->set_image( 0 );
 $obj->_get_gif_info;
 return $obj;
}

sub fast_forward {

    my( $self, $delta) = @_;

    $self->{_delta_} = $delta;
    if( not exists $self->{_playing_} ) {
	my $playing = exists $self->{'_NextId_'};
	$self->{_playing_} = $playing;
	$self->resume_animation if not $playing;
    } else {
	my $playing = delete $self->{_playing_};
	$self->pause_animation if not $playing;
    }

} # end fast_forward

*fast_reverse = \&fast_forward;

sub frame_count {
    my $frames = shift->{'_frames_'};
    return -1 unless $frames;
    return @$frames;
}

sub set_disposal_method {
    my( $self, $blank ) = @_;
    $blank = 1 if not defined $blank;
    $self->{_blank_} = $blank;
    $blank;
}

sub set_image
{
 my ($obj,$index)  = @_;
 my $frames = $obj->{'_frames_'};
 return unless $frames && @$frames;
 $index = 0 unless $index < @$frames;
 $obj->blank if $obj->{_blank_};  # helps some make others worse
 $obj->copy($frames->[$index]);
 $obj->{'_frame_index_'} = $index;
}

sub next_image
{
 my ($obj, $delta)  = @_;
 $obj->_next_image($delta);
}

sub _next_image
{
 my ($obj, $delta, $in_animation)  = @_;
 $delta = $obj->{_delta_} unless $delta;
 my $frames = $obj->{'_frames_'};
 return unless $frames && @$frames;
 my $next_index = (($obj->{'_frame_index_'} || 0) + $delta);
 if ($next_index > @$frames && $in_animation && $obj->{'_loop_'} ne 'forever')
  {
   return 0; # signal to stop animation
  }
 $next_index %= @$frames;
 $obj->set_image($next_index);
 1;
}

sub prev_image { shift->next_image( -1 ) }

sub next_image_in_animation
{
 my ($obj, $delta) = @_;
 my $continue = $obj->_next_image($delta, 1);
 if (!$continue && $self->{'_NextId_'})
  {
   $obj->pause_animation;
  }
}

sub pause_animation { 
    my $self = shift;
    my $id = delete $self->{'_NextId_'};
    Tk::catch { $id->cancel } if $id;
}

sub resume_animation {
    my( $self, $period ) = @_;
    if( not defined $self->{'_period_'} ) {
	$self->{'_period_'} = defined( $period ) ? $period : 100;
    }
    $period = $self->{'_period_'};
    my $w = $self->MainWindow;
    $self->{'_NextId_'} = $w->repeat( $period => [ $self => 'next_image_in_animation' ] );
}

sub start_animation
{
 my ($obj,$period) = @_;
 my $frames = $obj->{'_frames_'};
 return unless $frames && @$frames;
 my $w = $obj->MainWindow;
 $obj->stop_animation;
 $obj->{'_period_'} = $period if $period;
 $obj->{'_NextId_'} = $w->repeat($obj->{'_period_'},[$obj,'next_image_in_animation']);
}

sub stop_animation
{
 my ($obj) = @_;
 my $id = delete $obj->{'_NextId_'};
 Tk::catch { $id->cancel } if $id;
 $obj->set_image(0);
}

sub _get_gif_info
{
 my ($obj) = @_;
 my $info;
 if (defined(my $file = $obj->cget(-file)) && eval { require Image::Info; 1; })
  {
   $info = Image::Info::image_info($file);
  }
 elsif (defined(my $data = $obj->cget(-data)))
  {
   if ($data =~ m{^GIF8} && eval { require Image::Info; 1; })
    {
     $info = Image::Info::image_info(\$data);
    }
   elsif (eval { require Image::Info; require MIME::Base64; 1; })
    {
     $data = MIME::Base64::decode_base64($data);
     $info = Image::Info::image_info(\$data);
    }
  }
 if ($info)
  {
   $obj->{'_blank_'} = $info->{DisposalMethod} == 2 || $info->{DisposalMethod} == 3;
   $obj->{'_period_'} = $info->{Delay}*1000 if defined $info->{Delay};
   $obj->{'_loop_'} = $info->{GIF_Loop};
  }
 $obj->{'_blank_'} = 0 if !defined $obj->{'_blank_'};
 $obj->{'_period_'} = 100 if !defined $obj->{'_period_'};
 $obj->{'_loop_'} = 'forever' if !defined $obj->{'_loop_'};
 $obj->{'_delta_'} = 1;
}

1;

__END__

#
# This almost works for changing the animation on the fly
# but does not resize things correctly
#

sub gif_sequence
{
 my ($obj,%args) = @_;
 my $widget = $obj->MainWindow;
 my @images;
 local $@;
 while (1)
  {
   my $index = @images;
   $args{'-format'} = "gif -index $index";
   my $img;
   eval
    {local $SIG{'__DIE__'};
     my $img = $widget->Photo(%args);
     push(@images,$img);
    };
   last if $@;
  }
 if (@images)
  {
   delete $obj->{'_frames_'};
   $obj->add_frame(@images);
   $obj->configure(-width => 0, -height => 0);
   $obj->set_frame(0);
  }
}


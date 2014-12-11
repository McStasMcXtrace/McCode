package Tk::CmdLine; # -*-Perl-*-

#/----------------------------------------------------------------------------//
#/ Module: Tk/CmdLine.pm
#/
#/ Purpose:
#/
#/   Process standard X11 command line options and set initial resources.
#/
#/ Author: ????                      Date: ????
#/
#/ History: SEE POD
#/----------------------------------------------------------------------------//

use vars qw($VERSION);
$VERSION = '4.007'; # $Id: //depot/Tkutf8/Tk/CmdLine.pm#6 $

use 5.004;

use strict;

use Config;

my $OBJECT = undef; # define the current object

#/----------------------------------------------------------------------------//
#/ Constructor
#/   Returns the object reference.
#/----------------------------------------------------------------------------//

sub new # Tk::CmdLine::new()
{
    my $this  = shift(@_);
    my $class = ref($this) || $this;

    my $name = 'pTk';
    $name = $1 if (($0 =~ m/(?:^|[\/\\])([\w-]+)(?:\.\w+)?$/) && ($1 ne '-e'));

    my $self = {
        name        => $name,
        config      => { -name => $name },
        options     => {},
        methods     => {},
        command     => [],
        synchronous => 0,
        iconic      => 0,
        motif       => ($Tk::strictMotif || 0),
        resources   => {} };

    return bless($self, $class);
}

#/----------------------------------------------------------------------------//
#/ Process the arguments in a given array or in @ARGV.
#/   Returns the object reference.
#/----------------------------------------------------------------------------//

sub Argument_ # Tk::CmdLine::Argument_($flag) # private method
{
    my $self = shift(@_);
    my $flag = shift(@_);
    unless ($self->{offset} < @{$self->{argv}})
    {
        die 'Usage: ', $self->{name}, ' ... ', $flag, " <argument> ...\n";
    }
    return splice(@{$self->{argv}}, $self->{offset}, 1);
}

sub Config_ # Tk::CmdLine::Config_($flag, $name) # private method
{
    my $self = shift(@_);
    my ($flag, $name) = @_;
    my $val = $self->Argument_($flag);
    push(@{$self->{command}}, $flag, $val);
    $self->{config}->{"-$name"} = $val;
}

sub Flag_ # Tk::CmdLine::Flag_($flag, $name) # private method
{
    my $self = shift(@_);
    my ($flag, $name) = @_;
    push(@{$self->{command}}, $flag);
    $self->{$name} = 1;
}

sub Option_ # Tk::CmdLine::Option_($flag, $name) # private method
{
    my $self = shift(@_);
    my ($flag, $name) = @_;
    my $val = $self->Argument_($flag);
    push(@{$self->{command}}, $flag, $val);
    $self->{options}->{"*$name"} = $val;
}

sub Method_ # Tk::CmdLine::Method_($flag, $name) # private method
{
    my $self = shift(@_);
    my ($flag, $name) = @_;
    my $val = $self->Argument_($flag);
    push(@{$self->{command}}, $flag, $val);
    $self->{methods}->{$name} = $val;
}

sub Resource_ # Tk::CmdLine::Resource_($flag, $name) # private method
{
    my $self = shift(@_);
    my ($flag, $name) = @_;
    my $val = $self->Argument_($flag);
    if ($val =~ /^([^!:\s]+)*\s*:\s*(.*)$/)
    {
        push(@{$self->{command}}, $flag, $val);
        $self->{options}->{$1} = $2;
    }
}

my %Method = (
    background   => 'Option_',
    bg           => 'background', # alias
    class        => 'Config_',
    display      => 'screen',     # alias
    fg           => 'foreground', # alias
    fn           => 'font',       # alias
    font         => 'Option_',
    foreground   => 'Option_',
    geometry     => 'Method_',
    iconic       => 'Flag_',
    iconposition => 'Method_',
    motif        => 'Flag_',
    name         => 'Config_',
    screen       => 'Config_',
    synchronous  => 'Flag_',
    title        => 'Config_',
    xrm          => 'Resource_'
);

sub SetArguments # Tk::CmdLine::SetArguments([@argument])
{
    my $self = (@_ # define the object as necessary
        ? ((ref($_[0]) eq __PACKAGE__)
            ? shift(@_)
            : (($_[0] eq __PACKAGE__) ? shift(@_) : 1) && ($OBJECT ||= __PACKAGE__->new()))
        : ($OBJECT ||= __PACKAGE__->new()));
    $OBJECT = $self; # update the current object
    $self->{argv}   = (@_ ? [ @_ ] : \@ARGV);
    $self->{offset} = 0; # its existence will denote that this method has been called

    my @option = ();

    while ($self->{offset} < @{$self->{argv}})
    {
        last if ($self->{argv}->[$self->{offset}] eq '--');
        unless (
            (($self->{argv}->[$self->{offset}] =~ /^-{1,2}(\w+)$/)  && (@option = $1)) ||
            (($self->{argv}->[$self->{offset}] =~ /^--(\w+)=(.*)$/) && (@option = ($1, $2))))
        {
            ++$self->{offset};
            next;
        }

        next if (!exists($Method{$option[0]}) && ++$self->{offset});

        $option[0] = $Method{$option[0]} if exists($Method{$Method{$option[0]}});

        my $method = $Method{$option[0]};

        if (@option > 1) # replace --<option>=<value> with <value>
        {
            $self->{argv}->[$self->{offset}] = $option[1];
        }
        else # remove the argument
        {
            splice(@{$self->{argv}}, $self->{offset}, 1);
        }

        $self->$method(('-' . $option[0]), $option[0]);
    }

    $self->{config}->{-class} ||= ucfirst($self->{config}->{-name});

    delete($self->{argv}); # no longer needed

    return $self;
}

use vars qw(&process); *process = \&SetArguments; # alias to keep old code happy

#/----------------------------------------------------------------------------//
#/ Get a list of the arguments that have been processed by SetArguments().
#/   Returns an array.
#/----------------------------------------------------------------------------//

sub GetArguments # Tk::CmdLine::GetArguments()
{
    my $self = (@_ # define the object as necessary
        ? ((ref($_[0]) eq __PACKAGE__)
            ? shift(@_)
            : (($_[0] eq __PACKAGE__) ? shift(@_) : 1) && ($OBJECT ||= __PACKAGE__->new()))
        : ($OBJECT ||= __PACKAGE__->new()));
    $OBJECT = $self; # update the current object

    $self->SetArguments() unless exists($self->{offset}); # set arguments if not yet done

    return @{$self->{command}};
}

#/----------------------------------------------------------------------------//
#/ Get the value of a configuration option (default: -class).
#/   Returns the option value.
#/----------------------------------------------------------------------------//

sub cget # Tk::CmdLine::cget([$option])
{
    my $self = (@_ # define the object as necessary
        ? ((ref($_[0]) eq __PACKAGE__)
            ? shift(@_)
            : (($_[0] eq __PACKAGE__) ? shift(@_) : 1) && ($OBJECT ||= __PACKAGE__->new()))
        : ($OBJECT ||= __PACKAGE__->new()));
    $OBJECT = $self; # update the current object
    my $option = shift(@_) || '-class';

    $self->SetArguments() unless exists($self->{offset}); # set arguments if not yet done

    return (exists($self->{config}->{$option}) ? $self->{config}->{$option} : undef);
}

#/----------------------------------------------------------------------------//

sub CreateArgs # Tk::CmdLine::CreateArgs()
{
    my $self = (@_ # define the object as necessary
        ? ((ref($_[0]) eq __PACKAGE__)
            ? shift(@_)
            : (($_[0] eq __PACKAGE__) ? shift(@_) : 1) && ($OBJECT ||= __PACKAGE__->new()))
        : ($OBJECT ||= __PACKAGE__->new()));
    $OBJECT = $self; # update the current object

    $self->SetArguments() unless exists($self->{offset}); # set arguments if not yet done

    return $self->{config};
}

#/----------------------------------------------------------------------------//

sub Tk::MainWindow::apply_command_line
{
    my $mw = shift(@_);

    my $self = ($OBJECT ||= __PACKAGE__->new());

    $self->SetArguments() unless exists($self->{offset}); # set arguments if not yet done

    foreach my $priority (keys(%{$self->{resources}}))
    {
        foreach my $resource (@{$self->{resources}->{$priority}})
        {
            $mw->optionAdd(@{$resource}, $priority);
        }
    }

    foreach my $key (keys(%{$self->{options}}))
    {
        $mw->optionAdd($key => $self->{options}->{$key}, 'interactive');
    }

    foreach my $key (keys(%{$self->{methods}}))
    {
        $mw->$key($self->{methods}->{$key});
    }

    if ($self->{methods}->{geometry})
    {
        if ($self->{methods}->{geometry} =~ /[+-]\d+[+-]\d+/)
        {
            $mw->positionfrom('user');
        }
        if ($self->{methods}->{geometry} =~ /\d+x\d+/)
        {
            $mw->sizefrom('user');
        }
        delete $self->{methods}->{geometry}; # XXX needed?
    }

    $mw->Synchronize() if $self->{synchronous};

    if ($self->{iconic})
    {
        $mw->iconify();
        $self->{iconic} = 0;
    }

    $Tk::strictMotif = ($self->{motif} || 0);

    # Both these are needed to reliably save state
    # but 'hostname' is tricky to do portably.
    # $mw->client(hostname());
    $mw->protocol('WM_SAVE_YOURSELF' => ['WMSaveYourself',$mw]);
    $mw->command([ $self->{name}, @{$self->{command}} ]);
}

#/----------------------------------------------------------------------------//
#/ Set the initial resources.
#/   Returns the object reference.
#/----------------------------------------------------------------------------//

sub SetResources # Tk::CmdLine::SetResources((\@resource | $resource) [, $priority])
{
    my $self = (@_ # define the object as necessary
        ? ((ref($_[0]) eq __PACKAGE__)
            ? shift(@_)
            : (($_[0] eq __PACKAGE__) ? shift(@_) : 1) && ($OBJECT ||= __PACKAGE__->new()))
        : ($OBJECT ||= __PACKAGE__->new()));
    $OBJECT = $self; # update the current object

    $self->SetArguments() unless exists($self->{offset}); # set arguments if not yet done
    return $self unless @_;

    my $data      = shift(@_);
    my $priority  = shift(@_) || 'userDefault';

    $self->{resources}->{$priority} = [] unless exists($self->{resources}->{$priority});

    foreach my $resource ((ref($data) eq 'ARRAY') ? @{$data} : $data)
    {
        if (ref($resource) eq 'ARRAY') # resources in [ <pattern>, <value> ] format
        {
            push(@{$self->{resources}->{$priority}}, [ @{$resource} ])
                if (@{$resource} == 2);
        }
        else # resources in resource file format
        {
            push(@{$self->{resources}->{$priority}}, [ $1, $2 ])
                if ($resource =~ /^([^!:\s]+)*\s*:\s*(.*)$/);
        }
    }

    return $self;
}

#/----------------------------------------------------------------------------//
#/ Load initial resources from one or more files (default: $XFILESEARCHPATH with
#/ priority 'startupFile' and $XUSERFILESEARCHPATH with priority 'userDefault').
#/   Returns the object reference.
#/----------------------------------------------------------------------------//

sub LoadResources # Tk::CmdLine::LoadResources([%options])
{
    my $self = (@_ # define the object as necessary
        ? ((ref($_[0]) eq __PACKAGE__)
            ? shift(@_)
            : (($_[0] eq __PACKAGE__) ? shift(@_) : 1) && ($OBJECT ||= __PACKAGE__->new()))
        : ($OBJECT ||= __PACKAGE__->new()));
    $OBJECT = $self; # update the current object

    $self->SetArguments() unless exists($self->{offset}); # set arguments if not yet done

    my %options = @_;

    my @file = ();
    my $echo = (exists($options{-echo})
        ? (defined($options{-echo}) ? $options{-echo} : \*STDOUT) : undef);

    unless (%options && (exists($options{-file}) || exists($options{-symbol})))
    {
        @file = (
            { -symbol => 'XFILESEARCHPATH',     -priority => 'startupFile' },
            { -symbol => 'XUSERFILESEARCHPATH', -priority => 'userDefault' } );
    }
    else
    {
        @file = { %options };
    }

    my $delimiter = (($^O eq 'MSWin32') ? ';' : ':');

    foreach my $file (@file)
    {
        my $fileSpec = $file->{-spec} = undef;
        if (exists($file->{-symbol}))
        {
            my $xpath = undef;
            if ($file->{-symbol} eq 'XUSERFILESEARCHPATH')
            {
                $file->{-priority} ||= 'userDefault';
                foreach my $symbol (qw(XUSERFILESEARCHPATH XAPPLRESDIR HOME))
                {
                    last if (exists($ENV{$symbol}) && ($xpath = $ENV{$symbol}));
                }
                next unless defined($xpath);
            }
            else
            {
                $file->{-priority} ||= (($file->{-symbol} eq 'XFILESEARCHPATH')
                    ? 'startupFile' : 'userDefault');
                next unless (
                    exists($ENV{$file->{-symbol}}) && ($xpath = $ENV{$file->{-symbol}}));
            }

            unless (exists($self->{translation}))
            {
                $self->{translation} = {
                    '%l' => '',                       # ignored
                    '%C' => '',                       # ignored
                    '%S' => '',                       # ignored
                    '%L' => ($ENV{LANG} || 'C'),      # language
                    '%T' => 'app-defaults',           # type
                    '%N' => $self->{config}->{-class} # filename
                };
            }

            my @postfix = map({ $_ . '/' . $self->{config}->{-class} }
                ('/' . $self->{translation}->{'%L'}), '');

            ITEM: foreach $fileSpec (split($Config{path_sep}, $xpath))
            {
                if ($fileSpec =~ s/(%[A-Za-z])/$self->{translation}->{$1}/g) # File Pattern
                {
                    if (defined($echo) && ($file->{-symbol} ne 'XFILESEARCHPATH'))
                    {
                        print $echo 'Checking ', $fileSpec, "\n";
                    }
                    next unless ((-f $fileSpec) && (-r _) && (-s _));
                    $file->{-spec} = $fileSpec;
                    last;
                }
                else # Directory - Check for <Directory>/$LANG/<Class>, <Directory>/<CLASS>
                {
                    foreach my $postfix (@postfix)
                    {
                        my $fileSpec2 = $fileSpec . $postfix;
                        if (defined($echo) && ($file->{-symbol} ne 'XFILESEARCHPATH'))
                        {
                            print $echo 'Checking ', $fileSpec2, "\n";
                        }
                        next unless ((-f $fileSpec2) && (-r _) && (-s _));
                        $file->{-spec} = $fileSpec2;
                        last ITEM;
                    }
                }
            }
        }
        elsif (exists($file->{-file}) && ($fileSpec = $file->{-file}))
        {
            print $echo 'Checking ', $fileSpec, "\n" if defined($echo);
            next unless ((-f $fileSpec) && (-r _) && (-s _));
            $file->{-spec} = $fileSpec;
        }
    }

    foreach my $file (@file)
    {
        next unless defined($file->{-spec});
        local *SPEC;
        next unless open(SPEC,$file->{-spec});
        print $echo ' Loading ', $file->{-spec}, "\n" if defined($echo);

        my $resource     = undef;
        my @resource     = ();
        my $continuation = 0;

        while (defined(my $line = <SPEC>))
        {
            chomp($line);
            next if ($line =~ /^\s*$/); # skip blank lines
            next if ($line =~ /^\s*!/); # skip comments
            $continuation = ($line =~ s/\s*\\$/ /); # search for trailing backslash
            unless (defined($resource)) # it is the first line
            {
                $resource = $line;
            }
            else # it is a continuation line
            {
                $line =~ s/^\s*//; # remove leading whitespace
                $resource .= $line;
            }
            next if $continuation;
            push(@resource, [ $1, $2 ]) if ($resource =~ /^([^:\s]+)*\s*:\s*(.*)$/);
            $resource = undef;
        }

        close(SPEC);

        if (defined($resource)) # special case - EOF after line with trailing backslash
        {
            push(@resource, [ $1, $2 ]) if ($resource =~ /^([^:\s]+)*\s*:\s*(.*)$/);
        }

        $self->SetResources(\@resource, $file->{-priority}) if @resource;
    }

    return $self;
}

#/----------------------------------------------------------------------------//

1;

__END__

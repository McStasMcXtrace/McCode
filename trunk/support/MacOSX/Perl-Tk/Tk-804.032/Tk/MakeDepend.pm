package Tk::MakeDepend;
use strict;
use vars qw(%define);
use Config;

my @include;

use Carp;

$SIG{__DIE__} = \&Carp::confess;


use vars qw($VERSION);
$VERSION = '4.015';

sub scan_file;

sub do_include
{
 my ($inc,$dep,@include) = @_;
 foreach my $dir (@include)
  {
   my $path = "$dir/$inc";
   if (-f $path)
    {
     scan_file($path,$dep) unless exists $dep->{$path};
     return;
    }
  }
 warn "Cannot find '$inc' assume made\n";
 $dep->{$inc} = 1;
}

sub remove_comment
{
 s#^\s*/\*.*?\*/\s*##g;
}


sub term
{
 remove_comment();
 return !term() if s/^\s*!//;
 return exists($define{$1}) if s/^\s*defined\s*\(([_A-Za-z][_\w]*)\s*\)//;
 return exists($define{$1}) if s/^\s*defined\s*([_A-Za-z][_\w]*)//;
 return eval "$1" if s/^\s*(0x[0-9a-f]+)L?//i;
 return $1 if s/^\s*(\d+)L?//;
 return $define{$1} || 0 if s/^\s*([_A-Za-z][_\w]*)//;
 if (s/^\s*\(//)
  {
   my $val = expression(0);
   warn "Missing ')'\n" unless s/^\s*\)//;
   return $val;
  }
 warn "Invalid term:$_";
 return undef;
}

my %pri = ( '&&' => 4,
            '||' => 3,
            '>=' => 2, '<=' => 2, '<' => 2, '>' => 2,
            '==' => 1, '!=' => 1  );

sub expression
{
 my $pri = shift;
 # printf STDERR "%d# expr . $_\n";
 my $invert = 0;
 my $lhs = term() || 0;
 remove_comment();
 while (/^\s*(&&|\|\||>=?|<=?|==|!=)/)
  {
   my $op = $1;
   last unless ($pri{$op} >= $pri);
   s/^\s*\Q$op\E//;
   # printf STDERR "%d# $lhs $op . $_\n";
   my $rhs = expression($pri{$op}) || 0;
   my $e = "$lhs $op $rhs";
   $lhs = eval "$e" || 0;
   die "'$e' $@"  if $@;
   remove_comment();
  }
 return $lhs;
}

sub do_if
{
 my ($key,$expr) = @_;
 chomp($expr);
 if ($key eq 'ifdef' || $key eq 'ifndef')
  {
   if ($expr =~ /^\s*(\w+)/)
    {
     my $val = exists $define{$1};
     $val = !$val if ($key eq 'ifndef');
#    printf STDERR "%d from $key $expr\n",$val;
     return $val;
    }
  }
 else
  {
   local $_ = $expr;
   my $val = expression(0) != 0;
   warn "trailing: $_" if /\S/;
   #printf STDERR "%d from $key $expr\n",$val;
   return $val;
  }
}

sub scan_file
{
 no strict 'refs';
 my ($file,$dep) = @_;
 open($file,"<$file") || die "Cannot open $file:$!";
 local $_;
 my ($srcdir) = $file =~ m#^(.*)[\\/][^\\/]*$#;
 $srcdir = '.' unless defined $srcdir;
 my $live = 1;
 $dep->{$file} = 1;
 my @stack;
 while (<$file>)
  {
   $_ .= <$file> while (s/\\\n/ /);
   if (/^\s*#\s*(\w+)\s*(.*?)\s*$/)
    {
     my $ol = $live;
     my $key = $1;
     my $rest = $2;
     if ($key =~ /^if(.*)$/)
      {
       push(@stack,$live);
       $live &&= do_if($key,$rest);
      }
     elsif ($key eq 'elif')
      {
       $live = ($live) ? 0 : $stack[-1];
       $live &&= do_if('if',$rest);
      }
     elsif ($key eq 'else')
      {
       $live = ($live) ? 0 : $stack[-1];
      }
     elsif ($key eq 'endif')
      {
       if (@stack)
        {
         $live = pop(@stack);
        }
       else
        {
         die "$file:$.: Mismatched #endif\n";
        }
      }
     elsif ($live)
      {
       if ($key eq 'include')
        {
         do_include($1,$dep,$srcdir,@include) if $rest =~ /^"(.*)"/;
        }
       elsif ($key eq 'define')
        {
         if ($rest =~ /^\s*([_A-Za-z][\w_]*)\s*(.*)$/)
          {
           my $sym = $1;
           my $val = $2 || 1;
           $val =~ s#\s*/\*.*?\*/\s*# #g;
           $define{$sym} = $val;
          }
         else
          {
           warn "ignore '$key $rest'\n";
          }
        }
       elsif ($key eq 'undef')
        {
         if ($rest =~ /^\s*([_A-Za-z][\w_]*)/)
          {
           delete $define{$1};
          }
        }
       elsif ($key =~ /^(line|pragma)$/)
        {

        }
       else
        {
         warn "ignore '$key $rest'\n";
        }
      }
     # printf STDERR "$file:$.: %d $key $rest\n",$live if ($ol != $live);
    }
   else
    {
     # print if $live;
    }
  }
 close($file);
 if (@stack)
  {
   warn "$file:$.: unclosed #if\n";
  }
}

sub command_line
{
 @include = ();
 local %define = ('__STDC__' => 1 );
 my $data = '';
 my @files;
 while (@_ && $_[-1] !~ /^-/)
  {
   unshift(@files,pop(@_));
  }
 my $flags = $Config{ccflags};
 $flags =~ s/^\s+|\s+$//g;
 my @opt = (@_, split(/\s+/,$flags));
 while (@opt)
  {
   local $_ = shift(@opt);
   if (/^-I(.*)$/)
    {
     push @include,$1;
    }
   elsif (/^-D([^=]+)(?:=(.*))?$/)
    {
     $define{$1} = $2 || 1;
    }
   elsif (/^-U(.*)$/)
    {
     delete $define{$1};
    }
   elsif (/^(-.*)$/)
    {
     # Some option
     if ($opt[0] !~ /^-/)
      {
       # next arg does not start with '-' assume it
       # belongs to this option and discard it silently
       shift(@opt);
      }
    }
   else
    {
     # We got confused
     warn "Ignoring $1\n";
    }
  }
 # force /usr/include (and other system directories) to be last element of @include
 if ($Config{ccname} eq 'gcc')
  {
   push @include, parse_gcc_inc_search_list();
  }
 else
  {
   push @include, $Config{'usrinc'}
     if (defined $Config{'usrinc'} and $Config{'usrinc'} ne '');
  }
 # warn "Include:@include\n";
 while (@files)
  {
   local $_ = shift(@files);
   unless (/^(.*)\.[^\.]+$/)
    {
     warn "Skip $_";
     next;
    }
   local %define = %define;
   my $base = $1;
   my $file = $_;
   my %dep;
   warn "Finding dependencies for $file\n";
   scan_file($_,\%dep);
   my $str = "\n$base\$(OBJ_EXT) : $base.c";
   delete $dep{$file};
   my @dep = (sort(keys %dep));
   while (@dep)
    {
     my $dep = shift(@dep);
     $dep =~ s#^\./##;
     if (length($str)+length($dep) > 70)
      {
       $data .= "$str \\\n";
       $str = ' ';
      }
     else
      {
       $str .= ' ';
      }
     $str .= $dep;
    }
   $data .= "$str\n";
  }
 return $data;
}

sub parse_gcc_inc_search_list
{
 our @GCC_INC_SEARCH_LIST;
 return @GCC_INC_SEARCH_LIST if @GCC_INC_SEARCH_LIST;

 require File::Spec;
 my $cmd = "$Config{cc} -xc -E -v -";
 my @inc;
 eval
  {
   open my $fh, "-|", "$cmd 2>&1 <".File::Spec->devnull
     or die "Error while running $cmd: $!";

   my $in_search_list;
   while(<$fh>)
    {
     if (/^#include <\.\.\.> search starts here:/)
      {
       $in_search_list = 1;
      }
     elsif ($in_search_list)
      {
       last if /^End of search list/;
       chomp;
       s/^\s+//;
       push @inc, $_;
      }
    }
  };
 if ($@)
  {
   warn $@;
  }
 if (!@inc)
  {
   # Fallback: add /usr/include
   @inc = $Config{'usrinc'}
     if (defined $Config{'usrinc'} and $Config{'usrinc'} ne '');
  }
 @GCC_INC_SEARCH_LIST = @inc;
 @inc;
}

1;
__END__



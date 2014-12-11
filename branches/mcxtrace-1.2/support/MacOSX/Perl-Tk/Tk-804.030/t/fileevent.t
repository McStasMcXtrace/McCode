BEGIN { $^W = 1; $| = 1;}
use Test;

#!/usr/local/bin/perl -w
#
# Severely hacked test based on IPC example from
# Advanced Perl/Tk Programming.

use 5.005;
use Carp;
#use IPADM;
use IO::Socket;
use Tk;
use Tk::LabEntry;
use Tk::ROText;
use strict;
use vars qw/$EXIT $HN $ME $MW $PID/;
use subs qw/do_command fini init ipsnd list_subnet open_subnet
    pipe_in pipe_out read_sdb start_ipc_helper/;

use vars qw/$AUTHORIZED_CLIENTS $DAEMON_HOST $DAEMON_PORT $DEBUG
    $EOF $MOD_DB_PATH $ORGANIZATION $SDB_PATH $VERSION/;

BEGIN { plan test => 1}

if ($^O eq 'MSWin32' || $^O eq 'cygwin') {
    skip(1, 'Cannot test pipe/fork/exec/fileevent on Win32 systems.');
    CORE::exit();
}

$AUTHORIZED_CLIENTS = 'localhost|loopback'; # a regex
$DAEMON_HOST = 'localhost';
$DAEMON_PORT = 1234;
$DEBUG = 1;
$EOF = '_END_OF_INFORMATION_';
$ORGANIZATION = 'Subnet List for ACME Rocket Supply, Inc.';

init;
MainLoop;
fini;

sub do_command {

    # Issue a single IPADM command and wait for a reply. Using
    # pipes and fileevent() allows X11 events to continue flowing.

    pipe_out @_;
    return pipe_in;

} # end do_command

sub pipe_in {

    # Now that the IPADM command has been issued, keep sysread-ing
    # until the $EOF string is read, and return all the accumulated
    # data, excluding $EOF.

    my(@data, $sysbuf, $sysdata, $sysstat, $wait);

    $MW->fileevent(\*PR, 'readable' => sub {

#        print STDERR "IN: about to sysread ...\n" if $DEBUG;
	if ( ($Tk::VERSION cmp '800.015') <= 0 ) {
#	if ( defined($ARGV[0]) ) {
	    $sysbuf = <PR>;
#	    print STDERR "IN: sysbuf=$sysbuf\n" if $DEBUG;
	} else {
	    $sysstat = sysread PR, $sysbuf, 7;
	    die "ipadm: sysread error $!" unless defined $sysstat;
#	    print STDERR "IN: bytes=$sysstat\n" if $DEBUG;
	}
	$sysdata .= $sysbuf;
	if ($sysdata =~ /$EOF$/s) {
	    @data = split /\n/, $sysdata;
	    $#data--;		# throw $EOF away
	    $wait++;		# unblock waitVariable()
	}

    });

    $MW->waitVariable(\$wait);
    $MW->fileevent(\*PR, 'readable' => '');
#    print STDERR "IN: @data\n" if $DEBUG;

    @data;

} # end pipe_in

sub pipe_out {

    # Issue an IPADM command by syswrite-ing all the data plus
    # the terminating $EOF.

    return unless defined $_[0];

    my($bytes, $offset, $sysdata, $sysstat, $wait);

    $sysdata = join '', @_, "$EOF\n";
    $bytes = length $sysdata;
    $offset = 0;

    $MW->fileevent(\*PW, 'writable' => sub {

	while ($bytes > 0) {
	    $sysstat = syswrite PW, $sysdata, $bytes, $offset;
	    die "ipadm: syswrite error $!" unless defined $sysstat;
	    $bytes  -= $sysstat;
	    $offset += $sysstat;
	}
	$wait++;

    });

    $MW->waitVariable(\$wait);
    $MW->fileevent(\*PW, 'writable' => '');
#    print STDERR "OT: $sysdata\n" if $DEBUG;

} # end pipe_out

sub fini {
    kill 'SIGTERM', $PID;
    unlink '/tmp/ipadmh';
    exit $EXIT;
}

sub init {

    my $frog = defined $ARGV[0] ? '<HANDLE>' : 'sysread()';
    my $well = 'all is well';
    if ( (($Tk::VERSION cmp '800.015') <= 0) and ($frog eq 'sysread()') ) {
	$well = 'all is NOT well';
    }
    if ( (($Tk::VERSION cmp '800.015') >  0) and ($frog eq '<HANDLE>') ) {
	$well = 'all is NOT well';
    }

    open(HELPER, ">/tmp/ipadmh") or die "cannot write helper program: $!";
    while ($_ = <DATA>) {
	print HELPER;
    }
    close HELPER or die $!;
    chmod 0755, '/tmp/ipadmh';

    $MW = MainWindow->new;
    $MW->title('ipadm - Administer IP Nodes');
    $MW->iconname('ipadm');
    $MW->minsize(50, 50);
    $MW->geometry("+100+50");
    $MW->protocol('WM_DELETE_WINDOW' => \&fini);

    # Create the menubar and friends.

    my $menubar = $MW->Menu;
    $MW->configure(-menu => $menubar);

    my $file = $menubar->cascade(-label => '~File');
    my $edit = $menubar->cascade(-label => '~Edit');
    my $help = $menubar->cascade(-label => '~Help');

    $file->command(-label => 'Quit', -command => \&fini);
    $edit->command(-label => 'Fast Find', -command => [$MW => 'bell']);
    $help->command(-label => 'About', -command => sub {
	$MW->messageBox(-message => "ipadm $VERSION\n\n99/07/15")});

    # Create the subnets table, a list of hypertext links, and some tags
    # to highlight the active entry and a binding to load a subnet.

    my $t = $MW->Scrolled('ROText',
        qw/-width 80 -height 10 -relief ridge -scrollbars w/);
    $t->pack(qw/-padx 5 -pady 3 -fill both -expand 1/);

    $t->tagConfigure(qw/title -font/ => 'Helvetica 18 bold');
    $t->tagConfigure(qw/subnet -lmargin1 .5c -lmargin2 1c -foreground blue/);
    $t->tagConfigure(qw/hot -relief raised -borderwidth 1 -background green/);

    start_ipc_helper;

    $t->insert('end', "\n$ORGANIZATION\n\n", ['title']);

    # Get a list of subnets from ipadmd, sort the subnets by
    # IP number, and add the title string to the text widget,
    # tagged with the SDB file name.

    my($status, @subnet_list) = do_command "get_subnet_list\n";
    die "Cannot get SDB list" unless  $status =~ /OK/;

    foreach (sort numerically @subnet_list) {
	my($sdb, $title) = /^(\S+)\s+(.*)$/;
	$t->insert('end', "$title\n", ['subnet', $sdb]);
    }

    #$t->tagBind(qw/subnet <ButtonRelease-1>/ => \&open_subnet);

    my $last_hot = '';
    $t->tagBind(qw/subnet <Enter>/ => sub {
	my $text = shift;	# SUBTLE STATEMENT HERE (-:
	my($x, $y) = ($Tk::event->x, $Tk::event->y);
	$last_hot = $text->index("\@$x,$y linestart");
	$text->tagAdd('hot', $last_hot, "$last_hot lineend");
    });
    $t->tagBind(qw/subnet <Leave>/ => sub {
	shift->tagRemove(qw/hot 1.0 end/);
    });
    $t->tagBind(qw/subnet <Motion>/ => sub {
	my $text = shift;
	my($x, $y) = ($Tk::event->x, $Tk::event->y);
	my $new_hot = $text->index("\@$x,$y linestart");
	if ($new_hot ne $last_hot) {
	    $text->tagRemove(qw/hot 1.0 end/);
	    $text->tagAdd('hot', $new_hot, "$new_hot lineend");
	    $last_hot = $new_hot;
	}
    });

    chomp($HN = `hostname`);
    $ME = getlogin;

    # Sanity check, see if the 4th line was read.

    my $fourth = 'Subnet 128B, ACME Rubber Band Development';
    $MW->update;
    $MW->after(1000);
    my $data = $t->get('7.0', '7.0 lineend');
    #print "four=$fourth!\n";
    #print "data=$data!\n";
    $EXIT = ($data eq $fourth) ? 0 : 1;
    ok($data, $fourth);
    $MW->destroy;

} # end init

sub lsearch {                   # $o = lsearch $regexp, @list;

    # Search the list using the supplied regular expression and return it's
    # ordinal, or -1 if not found.

    my($regexp, @list) = @_;
    my($i);

    for ($i=0; $i<=$#list; $i++) {
        return $i if $list[$i] =~ /$regexp/;
    }
    return -1;

} # end lsearch

sub numerically {
    my($n1, $n2);
    ($n1) = $a =~ /Subnet_(\d+)/;
    ($n2) = $b =~ /Subnet_(\d+)/;
    if ($n1 != $n2) {
	$n1 <=> $n2;
    } else {
	$a cmp $b;
    }
}

sub start_ipc_helper {

    # Start a child process and use pipes to talk with it.  The child
    # uses sockets to talk to the remote IPADM daemon.

    $SIG{PIPE} = sub {print STDERR "ipadmh pipe failure.\n"; exit};

    pipe CR, PW or die "cr/pw pipe $!";
    pipe PR, CW or die "pr/cw pipe $!";

    if ($PID = fork) { # parent, ipadm/Tk
	close CR;
	close CW;
	PW->autoflush(1);
    } elsif (defined $PID) { # child, exec ipadmh
	close PR;
	close PW;
	open STDIN,  "<&CR" or die "STDIN  open $!";
	open STDOUT, ">&CW" or die "STDOUT open $!";
	open STDERR, ">&CW" or die "STDERR open $!";
	STDOUT->autoflush(1);
	STDERR->autoflush(1);
	exec($^X,"/tmp/ipadmh", $DAEMON_HOST, $DAEMON_PORT) or die "exec $!";
	die "exec warp $!";
    } else {
	die "fork $!";
    } # ifend fork

    my(@stat) = do_command undef;	# did helper make a connection?
    return if $stat[0] =~ /Connect OK/;

    $MW->messageBox(-message => "Cannot connect to remote IPADM daemon " .
		    "$DAEMON_HOST:$DAEMON_PORT.  Please try again later.",
		    -title => 'Daemon is Dead', -icon  => 'warning',
		    -type => 'OK');
    fini;

} # end start_ipc_helper
__DATA__
#!/usr/local/bin/perl

use 5.005;
#use IPADM;
use IO::Socket;
use strict;

my $EOF = '_END_OF_INFORMATION_';

do {print "Usage:  ipadmh host port\n"; exit} unless @ARGV == 2;

STDOUT->autoflush(1);           # unbuffer output
sub timeout {print "1 Socket Timeout\n$EOF\n"; $SIG{ALRM} = \&timeout}
$SIG{PIPE} = sub {print "2 Pipe Error.\n$EOF\n"};

my $sock = 'defined';
print +((defined $sock) ? "0 Connect OK" : "3 Connect Failed"), "\n$EOF\n";


while(<STDIN>) {
    last if /^$EOF$/;
}

my(@data) = ();
$SIG{ALRM} = \&timeout;	# reset handler
alarm 60;

@data = <<"END-DATA";
0 OK
Subnet_1.sdb    Subnet    1, ACME Bean Counting Department
Subnet_128A.sdb Subnet 128A, ACME Coil Spring Development
Subnet_128B.sdb Subnet 128B, ACME Rubber Band Development
Subnet_2.sdb    Subnet    2, ACME Purchasing Department
$EOF
END-DATA

alarm 0;
#print (/^$EOF$/ ? @data : "4 Daemon Failure\n$EOF\n");
print @data;
__END__
    print <<"END-HELP";

Run this program with or without a command line argument.  If
\$ARGV[0] is defined then <HANDLE> is used by the fileevent()
callback to read from the pipe.  If \$ARGV[0] is not defined,
then sysread() is used.

This is Tk version $Tk::VERSION, and we'll use $frog to read
from the pipe.

When all is well, you should see a MainWindow with the heading
"Subnet List for ACME Rocket Supply, Inc", followed by a list
of (4) subnets.

================================================================
=  With this combination of Tk $Tk::VERSION and $frog I suspect
=  $well!
================================================================

The subnet list comes from a helper program via a pipe - the
real helper program gets this data via a socket from a server,
but for this demo the subnet data is hardcoded in the helper.

Regardless, the parent Tk program execs the helper and first
"asks" for the subnet list by issuing a command on its write
pipe, which the helper reads on its STDIN.  For this demo, the
helper doesn't even look at what it reads but simply writes the
subnet list to its STDOUT.

Now for the bug:  the Perl/Tk parent needs to read this subnet
list from its input pipe, but the fileevent() *mechanism*
changed after Tk version 800.015.  The subroutine pipe_in() in
the Tk parent "ipadm" needs to read via a <HANDLE> for Tk800.015
and before, but sysread() for Tk800.018.

END-HELP


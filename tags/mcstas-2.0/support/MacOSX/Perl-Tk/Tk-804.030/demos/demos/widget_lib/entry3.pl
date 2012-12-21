# entry3.pl

use vars qw/$TOP/;

sub entry3 {

    # This demonstration script creates several entry widgets whose
    # permitted input is constrained in some way.  It also shows off a
    # password entry.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'Four different entries are displayed below. You can add characters by pointing, clicking and typing, though each is constrained in what it will accept. The first only accepts integers or the empty string (checking when focus leaves it) and will flash to indicate any problem. The second only accepts strings with fewer than ten characters and sounds the bell when an attempt to go over the limit is made. The third accepts US phone numbers, mapping letters to their digit equivalent and sounding the bell on encountering an illegal character or if trying to type over a character that is not a digit. The fourth is a password field that accepts up to eight characters (silently ignoring further ones), and displaying them as asterisk characters.',
        -title    => 'Constrained Entry Demonstration',
        -iconname => 'entry3',
    );


    my $lf1 = $TOP->Labelframe(-text => 'Integer Entry');
    my $e1;
    $e1 = $lf1->Entry(
        -validate        => 'focus',
        -validatecommand => sub {
	    my ($proposed, $changes, $current, $index, $type) = @_;
	    return not $proposed =~ m/[^\d]/g;
	},
    );
    $e1->configure(
        -invalidcommand =>
          [\&entry3_flash, $e1, $e1->cget(-fg), $e1->cget(-bg)],
    );
    $e1->pack(qw/-fill x -expand 1 -padx 1m -pady 1m/);

    my $lf2 = $TOP->Labelframe(-text => 'Length-Constrained Entry');
    my $e2 = $lf2->Entry(
        -validate        => 'key',
        -invalidcommand  => sub {$TOP->bell},
        -validatecommand => sub {
	    my ($proposed, $changes, $current, $index, $type) = @_;
	    return length($proposed) < 10 ? 1 : 0;
        },
    );
    $e2->pack(qw/-fill x -expand 1 -padx 1m -pady 1m/);

    # phone number entry
    #
    # Note that the source to this is quite a bit longer as the behaviour
    # demonstrated is a lot more ambitious than with the others.
    #
    # Mapping from alphabetic characters to numbers.  This is probably
    # wrong, but it is the only mapping I have; the UK doesn't really go
    # for associating letters with digits for some reason.

    my %phone_letters = qw/abc 2 def 3 ghi 4 jkl 5 mno 6 pqrs 7 tuv 8 wxyz 9/;
    my %l2n;			# letter to number mapping hash

    while (my ($k, $v) = each %phone_letters) {
	map {$l2n{$_} = $v} split '', $k;
    }

    my $lf3 = $TOP->Labelframe(-text => 'US Phone-Number Entry');
    my $e3_var = '1-(000)-000-0000';
    my $e3;
    $e3 = $lf3->Entry(
        -validate        => 'key',
        -invalidcommand  => sub {$TOP->bell},
        -textvariable    => \$e3_var,
    );
    $e3->configure(
        -validatecommand => [\&entry3_validate_phone, $e3, \%l2n],
    );
    $e3->pack(qw/-fill x -expand 1 -padx 1m -pady 1m/);
    # Click to focus goes to the first editable character...
    $e3->bind('<FocusIn>' => sub {
	my $e = shift;
	if ($Tk::event->d ne "NotifyAncestor") {
	    $e->icursor(3);
	    $e->afterIdle(sub {$e->selectionClear});
	}
    });
    $e3->bind('<Left>'  => \&entry3_phone_left);
    $e3->bind('<Right>' => \&entry3_phone_right);

    my $lf4 = $TOP->Labelframe(-text => 'Password Entry');
    my $e4 = $lf4->Entry(
        -validate        => 'key',
        -show            => '*',
        -validatecommand => sub {
	    my ($proposed, $changes, $current, $index, $type) = @_;
	    return length($proposed) <= 8 ? 1 : 0;
	},
    );
    $e4->pack(qw/-fill x -expand 1 -padx 1m -pady 1m/);

    my $f = $TOP->Frame;
    $f->lower;
    $lf1->grid($lf2, -in => $f, qw/-padx 3m -pady 1m -sticky ew/);
    $lf3->grid($lf4, -in => $f, qw/-padx 3m -pady 1m -sticky ew/);
    $f->gridColumnconfigure([0, 1], -uniform => 1);
    $f->pack(qw/-fill both -expand 1/);

} # end entry3

sub entry3_flash {
    my ($w, $fg, $bg) = @_;
    $w->focusForce;
    $w->{count} = 9 unless defined $w->{count};
    if ($w->{count} < 1) {
	$w->configure(-foreground => $fg, -background => $bg);
	$w->{count} = 9;
    } else {
	if ($w->{count} % 2) {
	    $w->configure(-foreground => $bg, -background => $fg);
	} else {
	    $w->configure(-foreground => $fg, -background => $bg);
	}
	$w->{count}--;
	$w->after(200 => [\&entry3_flash, $w, $fg, $bg]);
    }
} # end entry3_flash

sub entry3_phone_left {

    # Skip over fixed characters in a phone-number string when moving left.

    my $e = shift;
    my $index = $e->index('insert');
    if ($index == 8) {
	# Skip back two extra characters
	$index -= 2;
	$e->icursor($index);
    } elsif ($index == 7 or $index == 12) {
	# Skip back one extra character
	$index -= 1;
	$e->icursor($index);
    } elsif ($index <= 3) {
	# Can't move any further
	$e->bell;
	#return -code break
	$e->break;
    }

} # end entry3_phone_left

sub entry3_phone_right {

    # Skip over fixed characters in a phone-number string when moving right.

    my ($e, $add) = @_;

    $add = 0 unless defined $add;
    my $index = $e->index('insert');
    if ($index + $add == 5) {
	# Skip forward two extra characters
	$index += 2;
	$e->icursor($index);
    } elsif ($index + $add == 6 or $index + $add == 10) {
	# Skip forward one extra character
	$index++;
	$e->icursor($index);
    } elsif ($index + $add == 15 and not $add) {
	# Can't move any further
	$e->bell;
	#return -code break
	$e->break;
    }

} # end entry3_phone_right

sub entry3_validate_phone {

    # Checks that the replacement (mapped to a digit) of the given
    # character in an entry widget at the given position will leave a
    # valid phone number in the widget.

    my ($w, $l2n_ref, $proposed, $changes, $current, $index, $type) = @_;

    return 1 if $index == -1;
    my $val = $w->cget(-validate);
    $w->afterIdle(sub {
	$w->configure(
            -validate       => $val,
            -invalidcommand => sub {$w->bell},
        );
    });
    if (not ($index < 3 or $index == 6 or $index == 7 or $index == 11 or
	     $index > 15) and $changes =~ m/[0-9A-Z]/i) {
        $w->delete($index);
	$changes =~ s/$_/$l2n_ref->{$_}/ig foreach (keys %$l2n_ref);
	$w->insert($index, $changes);
        $w->afterIdle([\&entry3_phone_right, $w, -1]);
	return 1;
    }
    return 0;

} # end entry3_validate_phone;

1;

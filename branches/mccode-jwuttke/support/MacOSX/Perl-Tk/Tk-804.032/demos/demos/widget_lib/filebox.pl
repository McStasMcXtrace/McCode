# filebox.tcl --
#
# This demonstration script prompts the user to select a file.
#
# SCCS: @(#) filebox.tcl 1.3 97/03/02 16:22:36

use vars qw/$TOP/;

sub filebox {
    my $demo = shift;

    $TOP = $MW->WidgetDemo
      (
       -name     => $demo,
       -text     => "Enter a file name in the entry box or click on the \"Browse\" buttons to select a file name using the file selection dialog.",
       -title    => 'File box Demonstration',
       -iconname => 'filebox',
      );
    foreach my $i (qw(open save)) {
	my $f = $TOP->Frame;
	my $lab = $f->Label(-text => "Select a file to $i: ",
			    -anchor => 'e');
	my $ent = $f->Entry(-width => 20);
	my $but = $f->Button(-text => "Browse ...",
			     -command => sub { fileDialog($TOP, $ent, $i)});
	$lab->pack(-side => 'left');
	$ent->pack(-side => 'left',-expand => 'yes', -fill => 'x');
	$but->pack(-side => 'left');
	$f->pack(-fill => 'x', -padx => '1c', -pady => 3);
    }

    my $cbf = $TOP->Frame->pack(-fill => 'x', -padx => '1c', -pady => 3);
    my $fd;
    $cbf->Radiobutton
      (-text => 'FileSelect',
       -variable => \$fd,
       -value => 'FileSelect',
       -command => sub { local($^W) = 0;
			 require Tk::FileSelect;
			 Tk::FileSelect->import('as_default');
			 _removeCachedFileDialogs();
		     })->pack(-side => 'left');
    my $fdb = $cbf->Radiobutton
      (-text => 'FBox',
       -variable => \$fd,
       -value => 'FBox',
       -command => sub { local($^W) = 0;
			 require Tk::FBox;
			 Tk::FBox->import('as_default');
			 _removeCachedFileDialogs();
		     })->pack(-side => 'left');
    $fdb->invoke;

# XXX Motif style file box not implemented
#     unless (compare($tcl_platform{'platform'},'unix'))
#       {
# 	  $w->{'.strict'} = $w->Checkbutton('Name','strict','-text',"Use Motif Style Dialog",'-variable','tk_strictMotif','-onvalue',1,'-offvalue',0);
# 	  $w->{'.strict'}->pack('-anchor','c');
#       }
}

sub fileDialog {
    my $w = shift;
    my $ent = shift;
    my $operation = shift;
    my $types;
    my $file;
    #   Type names		Extension(s)	Mac File Type(s)
    #
    #---------------------------------------------------------
    @types =
      (["Text files",           [qw/.txt .doc/]],
       ["Text files",           '',             'TEXT'],
       ["Perl Scripts",         '.pl',		'TEXT'],
       ["C Source Files",	['.c', '.h']],
       ["All Source Files",     [qw/.tcl .c .h/]],
       ["Image Files",		'.gif'],
       ["Image Files",		['.jpeg', '.jpg']],
       ["Image Files",   	'',		[qw/GIFF JPEG/]],
       ["All files",		'*']
      );
    if ($operation eq 'open') {
	$file = $w->getOpenFile(-filetypes => \@types);
    } else {
	$file = $w->getSaveFile(-filetypes => \@types,
				-initialfile => 'Untitled',
				-defaultextension => '.txt');
    }
    if (defined $file and $file ne '') {
	$ent->delete(0, 'end');
	$ent->insert(0, $file);
	$ent->xview('end');
    }
}

sub _removeCachedFileDialogs {
    my $mw = $TOP->MainWindow;
    my $remove = sub {
	my $t = shift;
	return if (!UNIVERSAL::isa($t, "Tk::Toplevel"));
	delete $t->{'tk_getOpenFile'};
	delete $t->{'tk_getSaveFile'};
    };
    $remove->($mw);
    $mw->Walk($remove);
}

{
	package Tk::TBrowseEntry;
	use base qw(Tk::Derived Tk::BrowseEntry);
	Construct Tk::Widget 'TBrowseEntry';
	sub LabEntryWidget { "Entry" }
	sub Populate {
		my ($cw, $args) = @_;
		my $sub = $args->{'-browsecmd'};
		unless(defined($sub)) { $sub = {}};
		$cw->SUPER::Populate($args);
		$cw->Subwidget('entry')->bind('<Return>', $sub);
		$cw->Subwidget('entry')->bind('<FocusOut>', $sub);
		$cw->ConfigSpecs(
			-background => ['SELF', 'DESCENDANTS'],
			DEFAULT => [$cw->Subwidget('entry')],
		);
	}
}

{
	package Tk::OptionLine;

	use base qw(Tk::Derived Tk::Frame);
	
	use strict;

	Construct Tk::Widget 'OptionLine';

	sub Populate {
   	my ($cw,$args) = @_;

	   $cw->SUPER::Populate($args);
	   
	   my @padding = (
	   	-padx => 2,
	   	-pady => 2,
	   );

		my $b = $cw->Checkbutton(
			-anchor => 'w',
			-width => 10,
			-onvalue => 1,
			-offvalue => 0,
			-command => sub { $cw->togglestate },
		)->pack(@padding,
			-side => 'left',
		);
		$cw->Advertise('status' => $b);
		my $f = $cw->Frame(
		)->pack(@padding,
			-side => 'left',
			-fill => 'both',
			-expand => 1,
			-padx => 2,
			-pady => 2,
		);
		$cw->Advertise('fields' => $f);

		$cw->ConfigSpecs(
			-background => ['SELF', 'DESCENDANTS'],
			-borderwidth => [$cw, $f],
			-command => ['PASSIVE', undef, undef, sub {}],
			-relief => [$cw, $f],
			-text => [$b],
			-variable => [$b],
			DEFAULT => [$cw],
		);
		$cw->togglestate;
	}
	
	sub togglestate {
		my $cw = shift;
		my $v = $cw->Subwidget('status')->cget('-variable');
		my $dv = $$v;
		if ($dv) { 
			$cw->setstate('normal'); 
		} else {
			$cw->setstate('disabled');
		}
	}
	
	sub setstate {
		my ($cw, $state) = @_;
		my @w = $cw->Subwidget('fields')->children;
		foreach my $c (@w) {
			$c->configure(-state => $state);
		}
	}

}#end of package OptionLine

{
	package Tk::OptionColor;

	use base qw(Tk::Derived Tk::OptionLine);
	
	use strict;

	Construct Tk::Widget 'OptionColor';

	sub Populate {
   	my ($cw,$args) = @_;

	   $cw->SUPER::Populate($args);
	   
	   my @padding = (
	   	-padx => 2,
	   	-pady => 2,
	   );
		my $f = $cw->Subwidget('fields');
		my $value = '';
		my $v = $f->Entry(
			-textvariable => \$value,
			-width => 20,
		)->pack(@padding,
			-side => 'left',
			-expand => 1,
			-fill => 'x',
		);
		my $cmd = sub {
			my $c = $cw->cget('-command');
			&$c;
		};
		$v->bind('<Return>', $cmd);
		$v->bind('<FocusOut>', $cmd);
		$f->Button(
			-bitmap => '@' . Tk->findINC('cbxarrow.xbm'),
			-command => sub {
				if (my $c = $cw->chooseColor(-initialcolor => $value)) {
					$cw->content($c);
				}
			}
		)->pack(@padding,
			-side => 'left'
		)->pack(@padding,
			-side => 'left',
		);

		$cw->ConfigSpecs(
			-textvariable => ['PASSIVE', undef, undef, \$value],
			-background => ['SELF', 'DESCENDANTS'],
			DEFAULT => [$cw],
		);
	}
	
	sub content {
		my $cw = shift;
		my $v = $cw->cget('-textvariable');
		if (@_) { 
			$$v = shift;
			my $cmd = $cw->cget('-command');
			if (defined($cmd)) {
				&$cmd($$v);
			}
		}
		return $$v;
	}

}#end of package OptionColor

{
	package Tk::OptionFont;

	use base qw(Tk::Derived Tk::OptionLine);
	
	use strict;

	Construct Tk::Widget 'OptionFont';

	sub Populate {
   	my ($cw,$args) = @_;

	   $cw->SUPER::Populate($args);
	   
	   my @padding = (
	   	-padx => 2,
	   	-pady => 2,
	   );
		my $f = $cw->Subwidget('fields');
		my $family = '';
		my $size = '10';
		my $weight = 'normal';
		my $slant = 'roman';
		my @fonts = sort $cw->fontFamilies;
		my $cmd = sub {
			my $c = $cw->cget('-command');
			&$c;
		};
		my $v = $f->TBrowseEntry(
			-browsecmd => $cmd,
			-variable => \$family,
			-width => 20,
			-choices => [ @fonts ],
		)->pack(
			-side => 'left',
			-expand => 1,
			-fill => 'x',
		);
		my @sizes = qw(0 2 3 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
			23 24 25 26 27 28 29 30 33 34 36 40 44 48 50 56 64 72);
		$f->TBrowseEntry(
			-browsecmd => $cmd,
			-variable => \$size,
			-listwidth => 20,
			-width => 3,
			-choices => \@sizes,
		)->pack(#@padding,
			-side => 'left',
		);
		$f->Checkbutton(
			-command => $cmd,
			-variable => \$weight,
			-text => 'Bold',
			-onvalue => 'bold',
			-offvalue => 'normal',
		)->pack(#@padding,
			-side => 'left',
		);
		$f->Checkbutton(
			-variable => \$slant,
			-command => $cmd,
			-text => 'Italic',
			-onvalue => 'italic',
			-offvalue => 'roman',
		)->pack(#@padding,
			-side => 'left',
		);

		$cw->ConfigSpecs(
			-background => ['SELF', 'DESCENDANTS'],
			-familyvar => ['PASSIVE', undef, undef, \$family],
			-sizevar => ['PASSIVE', undef, undef, \$size],
			-slantvar => ['PASSIVE', undef, undef, \$slant],
			-weightvar => ['PASSIVE', undef, undef, \$weight],
			DEFAULT => [$cw],
		);
	}
	
	sub content {
		my $cw = shift;
		my %t = (
			'-family' => '-familyvar',
			'-size' => '-sizevar',
			'-slant' => '-slantvar',
			'-weight' => '-weightvar',
		);
		if (@_) {
			my $o = shift;
			my %args = (@$o);
			foreach my $k (keys %args) {
				my $op = $t{$k};
				my $v = $cw->cget($op);
				$$v  = $args{$k}
			}
			my $cmd = $cw->cget('-command');
			if (defined($cmd)) {
				&$cmd($o);
			}
		}
		my @res = ();
		foreach my $k (keys %t) {
			my $v = $cw->cget($t{$k});
			push @res, $k, $$v;
		};
		return \@res;
	}
	

}#end of package OptionFont

package Tk::RulesEditor;

use strict;
use base qw(Tk::Derived Tk::Toplevel);

Construct Tk::Widget 'RulesEditor';

use File::Basename;

require Tk::HList;
require Tk::Adjuster;

sub Populate {
	my ($cw,$args) = @_;
	
	my $widget = delete $args->{'-widget'};
	unless (defined($widget)) {
		$widget = $cw->parent;
	}
	$args->{'-title'} = $widget->cget('-syntax') . ' - Rules editor';
   $cw->SUPER::Populate($args);
	my @padding = (
		-padx => 2,
		-pady => 2,
	);
	my $synfr = $cw->Frame(
		-relief => 'groove',
		-borderwidth => 2,
	)->pack(@padding,
		-side => 'bottom',
		-fill => 'x',
	);
	$synfr->Button(
		-text => 'Close',
		-command => sub { $cw->destroy },
	)->pack(@padding,
		-side => 'right',
	);
	$synfr->Button(
		-text => 'Apply',
		-command => sub { $cw->apply }
	)->pack(@padding,
		-side => 'right',
	);
		
	my $tagfr = $cw->LabFrame(
		-label => 'Tag names',
		-labelside => 'acrosstop',
	)->pack(
		-side => 'left', 
		-fill => 'both',
	);
	my $taglist = $tagfr->Scrolled('HList',
		-browsecmd => sub { $cw->entryOpen(shift) },
		-columns => 1,
		-scrollbars => 'osoe',
	)->pack(@padding,
		-expand => 1,
		-fill => 'both',
	);
	my $rules = $widget->cget('-rules');
	unless (defined($rules)) {
		$rules = $widget->highlightPlug->rules;
	}
	foreach my $rl (@$rules) {
		my @r = @$rl;
		my $tag = shift @r;
		$taglist->add($tag, 
			-text => $tag, 
			-data => \@r,
		);
	}
	my $oppn = $cw->LabFrame(
		-label => 'Options',
		-labelside => 'acrosstop',
	)->pack(
		-side => 'left',
		-expand => 1,
		-fill => 'both',
	);
	my $df = $oppn->Frame(
		-borderwidth => 2,
		-relief => 'groove',
	)->pack(@padding,
		-side => 'bottom', 
		-expand => 1,
		-fill => 'both',
	);
	my $d = $df->Entry(
	)->pack(@padding,
		-expand => 1,
		-fill => 'both'
	);
	$cw->Advertise('display' => $d);
	my @fields  = ('-foreground', '-background');

	my %flags = ();
	foreach my $fld (@fields) {
		my $flag = 0;
		$flags{$fld} = \$flag,
		my $b = $oppn->OptionColor(
			-variable => \$flag,
			-class => 'Frame',
			-borderwidth => 2,
			-relief => 'groove',
			-command => sub {
				$cw->entryOpen($cw->cget('-current'));
			},
			-text => $fld,
		)->pack(@padding, -fill => 'x');
		$b->togglestate;
		$cw->Advertise($fld => $b);
	}
	my $fl = 0;
	$flags{'-font'} = \$fl;
	my $font = $oppn->OptionFont(
		-variable => \$fl,
		-class => 'Frame',
		-borderwidth => 2,
		-relief => 'groove',
		-text => '-font',
		-command => sub {
			$cw->entryOpen($cw->cget('-current'));
		},
	)->pack(@padding, -fill => 'x');
	$font->togglestate;
	$cw->Advertise('-font' => $font);
	$cw->ConfigSpecs(
		-background => ['SELF', 'DESCENDANTS'],
		-current	=> ['PASSIVE', undef, undef, ''],
		-flags	=> ['PASSIVE', undef, undef, \%flags],
		-widget => ['PASSIVE', undef, undef, $widget],
		-sampletext => [{-text => $d}, 'sampletext', 'Sampletext', 'ABCDEFGHIJabcdefghij0123456789'],
		DEFAULT => ['SELF'],
	);
	$cw->Delegates(
		'DEFAULT' => $taglist,
	);
}

sub apply {
	my $cw = shift;
	my @tree = $cw->infoChildren;
	my @res = ();
	foreach my $c (@tree) {
		my $d = $cw->entrycget($c, '-data');
		push @res, [$c, @$d];
	}
	my $widget = $cw->cget('-widget');
	$widget->configure(-rules => \@res);
	$widget->rulesSave;
	$widget->highlightPlug;
}

sub entryClose {
	my $cw = shift;
	my $s = $cw->cget('-current');
	if ($s) {
		$cw->entrySync;
		$cw->configure('-current' => '');
		my @l = ('-foreground', '-background', '-font');
		foreach my $o (@l) {
			my $w = $cw->Subwidget($o);
			my $flags = $cw->cget('-flags');
			my $flag = $flags->{$o};
			$$flag = 0;
			my $widget = $cw->cget('-widget');
			my $display = $cw->Subwidget('display');
			if ($o eq '-font') {
				my $font = $widget->cget('-font');
				my $f = [
					-family => $widget->fontActual($font, '-family'),
					-size => abs($widget->fontActual($font, '-size')),
					-weight => $widget->fontActual($font, '-weight'),
					-slant => $widget->fontActual($font, '-slant'),
				];
				$w->content($f);
				$display->configure('-font' => $f);
			} else {
				my $c = $widget->cget($o);
				$w->content($c);
				$display->configure($o => $c);
			}
			$w->togglestate;
		}
	}	
}

sub entryOpen {
	my ($cw, $entry) = @_;
	$cw->entryClose;
	if ($entry) {
		my $data = $cw->entrycget($entry, '-data');
		my %opt = (@$data);
		foreach my $o (keys %opt) {
			my $w = $cw->Subwidget($o);
			my $flags = $cw->cget('-flags');
			my $flag = $flags->{$o};
			$$flag = 1;
			$w->content($opt{$o});
			$w->togglestate;
		}
		$cw->Subwidget('display')->configure(%opt);
		$cw->configure('-current' => $entry);
	}
}

sub entrySync {
	my $cw = shift;
	my $c = $cw->cget('-current');
	if ($c) {
		my @l = ('-foreground', '-background', '-font');
		my @data = ();
		foreach my $i (@l) {
			my $w = $cw->Subwidget($i);
			my $flag = $w->cget('-variable');
			my $f = $$flag;
			if ($f) {
				push @data, $i;
				push @data, $w->content;
			}
		};
		$cw->entryconfigure($c, -data => \@data);
	}
}

1;
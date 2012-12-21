# $Revision: 1.32 $, $Date: 2004/05/06 18:20:50 $
#
# Conditional build:
%bcond_without	tests	# do not perform "make test"
#
%include	/usr/lib/rpm/macros.perl
Summary:	PGPLOT perl module
Summary(pl):	Modu³ perla PGPLOT
Name:		perl-PGPLOT
Version:	2.21
Release:	5
# same as perl
License:	GPL v1+ or Artistic
Group:		Development/Languages/Perl
Source0:	http://www.cpan.org/modules/by-module/PGPLOT/PGPLOT-%{version}.tar.gz
# Source0-md5:	0c27c49f6443eb4fdcc9eaee4c756c87
BuildRequires:	xorg-x11-proto-devel
BuildRequires:	perl-devel >= 1:5.8.0
BuildRequires:	perl-ExtUtils-F77 >= 1.11
BuildRequires:	pgplot-devel >= 5.2.2-1
BuildRequires:  perl(ExtUtils::MakeMaker)
BuildRequires:  gcc-gfortran
Requires:       perl(:MODULE_COMPAT_%(eval "`%{__perl} -V:version`"; echo $version))
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Autoreq: no

%description
PGPLOT - perl interface to the PGPLOT graphics library.

%description -l pl
PGPLOT - interfejs perla do biblioteki graficznej PGPLOT.

%prep
%setup -q -n PGPLOT-%{version}

%build
%{__perl} Makefile.PL \
	INSTALLDIRS=vendor
%{__make}

%{?with_tests:echo /NULL | %{__make} test}

%install
rm -rf $RPM_BUILD_ROOT

%{__make} install \
	DESTDIR=$RPM_BUILD_ROOT

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(644,root,root,755)
/usr/lib64/perl5/perllocal.pod
/usr/lib64/perl5/vendor_perl/auto/PGPLOT/.packlist
%doc CHANGES README HELP
%{perl_vendorarch}/PGPLOT.pm
%dir %{perl_vendorarch}/auto/PGPLOT
%{perl_vendorarch}/auto/PGPLOT/PGPLOT.bs
%attr(755,root,root) %{perl_vendorarch}/auto/PGPLOT/PGPLOT.so
%{_mandir}/man3/*

%define date	%(echo `LC_ALL="C" date +"%a %b %d %Y"`)
%changelog
* %{date} PLD Team <feedback@pld-linux.org>
All persons listed below can be reached at <cvs_login>@pld-linux.org

$Log: perl-PGPLOT.spec,v $
Revision 1.32  2004/05/06 18:20:50  ankry
- fixed perl-devel BR

Revision 1.31  2004/05/06 05:46:53  ankry
- license fix, cosmetics

Revision 1.30  2004/04/27 15:26:24  ankry
- new bconds

Revision 1.29  2003/09/09 10:24:44  ankry
- spaces -> tab

Revision 1.28  2003/08/18 08:09:08  gotar
- mass commit: cosmetics (removed trailing white spaces)

Revision 1.27  2003/05/28 13:00:57  malekith
- massive attack: source-md5

Revision 1.26  2003/05/25 06:24:53  misi3k
- massive attack s/pld.org.pl/pld-linux.org/

Revision 1.25  2003/04/11 18:59:26  radek
- BR: perl-devel; do not BR: perl

Revision 1.24  2003/03/25 17:04:19  qboosh
- added tests, BR: fixed pgplot-devel; now it should build correctly
- release 5

Revision 1.23  2003/03/01 20:22:19  radek
- should be ready for perl-5.8.0; release++

Revision 1.22  2003/02/25 00:41:04  radek
- use the __perl macro

Revision 1.21  2003/02/18 15:37:35  ankry
- massive attack: change CPAN URLs

Revision 1.20  2002/11/26 00:14:36  ankry
- Massive attack: new %%doc, use %%{_examplesdir} and proper attrs therein

Revision 1.19  2002/05/11 14:28:11  kloczek
- s/ftp.perl.org/ftp.cpan.org/ in Source url.

Revision 1.18  2002/03/03 20:29:06  marcus
- rel.3.

Revision 1.17  2002/02/22 23:29:31  kloczek
- removed all Group fields translations (oure rpm now can handle translating
  Group field using gettext).

Revision 1.16  2002/02/10 15:32:57  depesz
adapterized

Revision 1.15  2002/01/18 02:14:24  kloczek
perl -pi -e "s/pld-list\@pld.org.pl/feedback\@pld.org.pl/"

Revision 1.14  2001/04/17 12:14:12  baggins
- release 2
- rebuild with perl 5.6.1

Revision 1.13  2001/04/14 18:00:09  baggins
- removed explicit requirements for perl = %%{version} and %%{perl_sitearch}
  they will be added by rpm if needed

Revision 1.12  2001/02/05 21:28:25  kloczek
- updated to 2.18.

Revision 1.11  2001/01/22 20:59:03  kloczek
- release 2,
- rebuild against perl 5.6,
- spec adapterized.

Revision 1.10  2000/11/17 15:10:13  kloczek
- use new rpm automation.

Revision 1.9  2000/11/16 11:58:20  cieciwa
- updated to version 2.17.

Revision 1.8  2000/06/09 07:23:40  kloczek
- added using %%{__make} macro.

Revision 1.7  2000/05/21 20:20:01  kloczek
- spec adapterized.

Revision 1.6  2000/04/14 09:30:02  qboosh
- added gcc-g77 to BuildRequires

Revision 1.5  2000/04/01 11:15:24  zagrodzki
- changed all BuildRoot definitons
- removed all applnkdir defs
- changed some prereqs/requires
- removed duplicate empty lines

Revision 1.4  2000/03/28 16:54:58  baggins
- translated kloczkish into english

Revision 1.3  1999/11/17 12:16:42  pius
- added BuildRequires: perl-ExtUtils-F77 >= 1.11

Revision 1.2  1999/11/13 20:17:26  pius
- fixed Source url

Revision 1.1  1999/11/13 16:48:05  pius
- initial release

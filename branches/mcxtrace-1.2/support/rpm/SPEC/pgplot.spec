%{!?tcl_version: %define tcl_version %(echo 'puts $tcl_version' | tclsh)}
%{!?tcl_sitearch: %define tcl_sitearch %{_libdir}/tcl%{tcl_version}}
Name: pgplot 
%define lvmajor 5
Version: 5.2.2
Release: 28%{?dist}
Summary: Graphic library for making simple scientific graphs

Group: Development/Libraries
License: freely available for non-commercial use

URL: http://www.astro.caltech.edu/~tjp/pgplot
Source0: ftp://ftp.astro.caltech.edu/pub/pgplot/pgplot5.2.tar.gz
Source1: pgplot.pc
Source2: cpgplot.pc
Source3: tk-pgplot.pc
Source4: pgplot-pkgIndex.tcl

# Make pgplot find files in standard locations such as
# /usr/libexec/pgplot and /usr/share/pgplot
Patch0: pgplot5.2-fsstnd.patch
# Fix the location of perl 
Patch1: pgplot5.2-makefile.patch
# make the compiler script accept FFLAGS and FC
Patch2: pgplot5.2-g77_gcc_conf.patch
# Needed by the png driver
Patch3: pgplot5.2-pngdriver.patch
# Needed to have a loadable tcl package
Patch4: pgplot5.2-tclpackage.patch

Buildroot: %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

BuildRequires: libpng-devel tk-devel libX11-devel gcc-gfortran
BuildRequires: perl texlive-latex glibc-common

Requires(post): /sbin/ldconfig
Requires(postun): /sbin/ldconfig

%description
The PGPLOT Graphics Subroutine Library is a Fortran- or C-callable, 
device-independent graphics package for making simple scientific graphs. 
It is intended for making graphical images of publication quality with 
minimum effort on the part of the user. For most applications, 
the program can be device-independent, and the output can be directed to 
the appropriate device at run time.

%package devel
Summary: Libraries, includes, etc. used to develop an application with %{name}
Group: Development/Libraries
Requires: %{name} = %{version}-%{release}
Requires: libX11-devel libpng-devel pkgconfig

%description devel
These are the header files and static libraries needed to develop a %{name} 
application.

%package demos
Summary: Demo applications of %{name}
Group: Development/Libraries
Requires: %{name} = %{version}-%{release}
%description demos
Demonstration applications for PGPLOT, a FORTRAN-callable,
device-independent graphics package for making simple scientific graphs.

%package -n tcl-%{name}
Summary: Tcl/Tk driver for %{name}
Group: Development/Libraries
Requires: %{name} = %{version}-%{release}
Requires: tcl(abi) = 8.5 
Provides: tk-%{name} = %{version}-%{release}

%description -n tcl-%{name}
Tcl/Tk driver for %{name}

%package -n tcl-%{name}-devel
Summary: Tcl/Tk driver for %{name} devel files 
Group: Development/Libraries
Requires: tcl-%{name} = %{version}-%{release}
Requires: %{name}-devel = %{version}-%{release}
Requires: tk-devel
Provides: tk-%{name}-devel = %{version}-%{release}

%description -n tcl-%{name}-devel
Libraries, includes, etc. used to develop an application using
the %{name} Tcl/Tk driver.

%prep
%setup -q -n %{name}

%patch0 -p1
%patch1 -p1
%patch2 -p1
%patch3 -p1
%patch4 -p1

%{__cp} %{SOURCE1} .
%{__cp} %{SOURCE2} .
%{__cp} %{SOURCE3} .
%{__cp} %{SOURCE4} pkgIndex.tcl

# Enabling the following drivers:
# PNG, PS, PPM, TCL/TK and X
%{__sed} \
-e 's/! PNDRIV/  PNDRIV/g' \
-e 's/! PSDRIV/  PSDRIV/g' \
-e 's/! PPDRIV/  PPDRIV/g' \
-e 's/! XWDRIV/  XWDRIV/g' \
-e 's/! TKDRIV/  TKDRIV/g' -i drivers.list

# This version of fedora includes gcc-fedora 4.3 
# that can compile the GIF driver
%if 0%{?fedora} >= 9
%{__sed} -e 's/! GIDRIV/  GIDRIV/g' -i drivers.list
%endif

# Creating pkgconfig files from templates
%{__sed} -e 's|archlibdir|%{_libdir}|g' -i pgplot.pc
%{__sed} -e 's|archlibdir|%{_libdir}|g' -i cpgplot.pc
%{__sed} -e 's|archlibdir|%{_libdir}|g' -i tk-pgplot.pc
%{__sed} -e 's|archlibdir|%{_libdir}|g' -i pkgIndex.tcl

# Version files stored in one Changelog
(for i in $(find . -name "ver*.txt" |sort -r); do iconv -f "ISO-8859-1" -t "utf8" $i; done) > ChangeLog

%build
./makemake . linux g77_gcc
# Parallel make not supported
%{__make} FC="f95" CC="%{__cc}" CFLAGS="%{optflags}" FFLAGS="%{optflags}" \
   NLIBS="-lgfortran -lm -lX11 -lpng"

# Creating dynamic library for C
%{__make}  %{?_smp_mflags} \
   FC=f95 CC="%{__cc}" CFLAGS="%{optflags}" FFLAGS="%{optflags}" cpg
%{__ar} x libcpgplot.a
%{__cc} %{optflags} -shared -o libc%{name}.so.%{version} \
    -Wl,-soname,libc%{name}.so.%{lvmajor} \
    cpg*.o -L . -l%{name} -lgfortran -lm -lX11 -lpng

# Creating dynamic library for TK
%{__ar} x libtkpgplot.a
%{__cc} %{optflags} -shared -o libtk%{name}.so.%{version} \
    -Wl,-soname,libtk%{name}.so.%{lvmajor} \
    tkpgplot.o -L . -l%{name} -ltk -ltcl -lX11 

for i in lib*.so.%{version}; do
  chmod 755 $i
done

%{__ln_s} lib%{name}.so.%{version} lib%{name}.so.%{lvmajor}
%{__ln_s} lib%{name}.so.%{version} lib%{name}.so
%{__ln_s} libc%{name}.so.%{version} libc%{name}.so.%{lvmajor}
%{__ln_s} libc%{name}.so.%{version} libc%{name}.so
%{__ln_s} libtk%{name}.so.%{version} libtk%{name}.so.%{lvmajor}
%{__ln_s} libtk%{name}.so.%{version} libtk%{name}.so

%{__make} %{?_smp_mflags} pgplot-routines.tex
%{__make} %{?_smp_mflags} pgplot.html
#pdflatex pgplot-routines.tex

%install
%{__rm} -rf %{buildroot}
%{__mkdir_p} %{buildroot}/%{_bindir}
%{__mkdir_p} %{buildroot}/%{_libdir}/pkgconfig
%{__mkdir_p} %{buildroot}/%{_includedir}
%{__mkdir_p} %{buildroot}/%{_datadir}/%{name}
%{__mkdir_p} %{buildroot}/%{_libexecdir}/%{name}
%{__mkdir_p} %{buildroot}/%{tcl_sitearch}/%{name}
%{__cp} -a  lib*%{name}.so* %{buildroot}/%{_libdir}
%{__install} -p -m 644 cpgplot.h %{buildroot}/%{_includedir}
%{__install} -p -m 644 tkpgplot.h %{buildroot}/%{_includedir}
%{__install} -p -m 644 rgb.txt %{buildroot}/%{_datadir}/%{name}
%{__install} -p -m 644 grfont.dat %{buildroot}/%{_datadir}/%{name}
%{__install} -p -m 755 pgxwin_server %{buildroot}/%{_libexecdir}/%{name}
%{__install} -p -m 755 pgdemo* cpgdemo %{buildroot}/%{_bindir}
%{__install} -p -m 644 *.pc %{buildroot}/%{_libdir}/pkgconfig
%{__install} -p -m 644 pkgIndex.tcl %{buildroot}/%{tcl_sitearch}/%{name}

%clean
%{__rm} -rf %{buildroot}

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%post -n tcl-%{name} -p /sbin/ldconfig

%postun -n tcl-%{name} -p /sbin/ldconfig

%files
%defattr(-,root,root,-)
%doc copyright.notice ChangeLog
%{_libdir}/lib%{name}.so.*
%{_libdir}/libc%{name}.so.*
%{_datadir}/%{name}
%{_libexecdir}/%{name}

%files devel
%defattr (-,root,root,-)
#%doc aaaread.me pgplot-routines.pdf pgplot.html copyright.notice
%{_libdir}/lib%{name}.so
%{_libdir}/libc%{name}.so
%{_includedir}/cpgplot.h
%{_libdir}/pkgconfig/pgplot.pc
%{_libdir}/pkgconfig/cpgplot.pc

%files -n tcl-%{name}
%defattr (-,root,root,-)
%doc copyright.notice
%{_libdir}/libtk%{name}.so.*
%{tcl_sitearch}/%{name}

%files -n tcl-%{name}-devel
%defattr (-,root,root,-)
%doc copyright.notice
%{_libdir}/libtk%{name}.so
%{_includedir}/tkpgplot.h
%{_libdir}/pkgconfig/tk-pgplot.pc

%files demos
%defattr (-,root,root,-)
%doc copyright.notice
%{_bindir}/*

%changelog
* Tue Nov 18 2008 Sergio Pascual <sergio.pasra@gmail.com> 5.2.2-28
- Fixing bz #168. There was a typo in patch0

* Thu Nov 13 2008 Sergio Pascual <sergio.pasra@gmail.com> 5.2.2-27
- Patch0 adapted to the buildsystem of fedora 10

* Thu Nov 13 2008 Sergio Pascual <sergio.pasra@gmail.com> 5.2.2-26
- Provides includes release

* Wed Nov 12 2008 Sergio Pascual <sergio.pasra@gmail.com> 5.2.2-25
- Fixed non-standard-executable-perm in libcpgplot and libtkpgplot
- Included version files in one Changelog
- Parallel make works only with make cpg
- Removed macro lvfull, using version instead
- copyright.notice in all subpackages
- Patched tk driver to have a loadable package
- Tk driver follows tcl guidelines

* Sun Nov 09 2008 Sergio Pascual <sergio.pasra@gmail.com> 5.2.2-24
- Using sed to enable the drivers instead of patch
- Removed docs package
- Added pkgconfig files
- Package tcl renamed as tk
- Create pdf doc from latex routines description

* Wed Nov 05 2008 Sergio Pascual <sergio.pasra@gmail.com> 5.2.2-23
- Fixing weak symbols in libcpgplot
- Poststages for libtkpgplot
- pgplot-tcl-devel requires pgplot-tcl

* Fri Jun 27 2008 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-22
- Spec cleanup
- Building againts el5

* Thu Jun 05 2008 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-21
- Readded gif
- Spec cleanup

* Thu Jun 05 2008 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-21
- Readded gif
- Spec cleanup

* Thu Sep 06 2007 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-20
- Adding png

* Fri Jul 20 2007 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-19
- Demos compiled with debug flags
- Test to compile with gfortran

* Thu Dec 14 2006 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-18
- Added Tcl/Tk driver. 
- New subpackges pgplot-tcl and pgplot-tcl-devel.

* Wed Nov 01 2006 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-17
- Rebuild for fc6.

* Fri Oct 13 2006 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-16
- Added correct Requires in devel subpackage.

* Mon Sep 25 2006 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-15
- Extra documentation added.

* Fri Jul 28 2006 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-14
- New m4 macros.

* Wed Jul 26 2006 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-13
- Honors FFLAGS and CFLAGS, can not be built with gfortran.
- Rebuild for FC5.

* Fri Mar 17 2006 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-12
- Removed pgplot-acentos.tar.gz.

* Wed Mar 15 2006 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-11
- Unpacking correctly pgplot-m4.tar.gz and pgplot-acentos.tar.gz.

* Wed Mar 15 2006 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-10
- Added Requires for devel package.

* Tue Feb 21 2006 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-9
- Minor fixes.
- Excluded static libraries.

* Tue Nov 28 2005 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-8
- Adding soname to the shared libs.
- Minor fixes.

* Wed Apr 27 2005 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-7
- Using dist tags. Adding pgplot.m4 and grfont with accented characters.

* Wed Apr 20 2005 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-6
- Rebuild for FC3. Removed grfont with accented characters.

* Fri Nov 21 2003 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-5
- Added docs package.

* Tue Feb 25 2003 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-4
- Added demos.

* Thu Feb 12 2003 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-3
- Splited devel part.

* Tue Feb 11 2003 Nicolas Cardiel <ncl@astrax.fis.ucm.es> 5.2.2-2
- Added suport for accents.

* Mon May 27 2002 Sergio Pascual <spr@astrax.fis.ucm.es> 5.2.2-1
- Initial RPM release.

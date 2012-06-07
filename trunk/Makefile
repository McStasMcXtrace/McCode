# Makefile for McStas.
#
#   This file is part of the McStas ray-trace simulation package
#   Copyright (C) 1997-2008, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; version 2 of the License.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# Available methods for installation
# make                 normal build
# make install         normal installation
# make clean           clean distro
# make test            distro self-test
# make plotter         best plotter choice
# make pgplot          plotter PGPLOT selection
# make reconfigure     reconfigure mcstas installation (after software update)
# make install-pgplot  build and install PGPLOT (including perl-PGPLOT)
# make install-scilab  build and install Scilab
# make install-apps    install highligh syntax in gedit/kate 
#                          and icon in menu/education (requires administrator permissions)

SHELL = /bin/sh

prefix = /usr/local
exec_prefix = ${prefix}
bindir = ${exec_prefix}/bin
srcdir = .
libdir = ${exec_prefix}/lib
mandir = ${prefix}/share/man
PWD = `pwd`


DEBUG = -DDEBUG=0
libdir_mcstas = $(libdir)/mcstas

CC = gcc
MINGW = no
CFLAGS = -g -O2
LDFLAGS= 

HAVE_QSORT = @HAVE_QSORT@
USE_NEXUS = 

DEFS = McStas -DPACKAGE_NAME=\"@MCCODE_NAME@\" -DPACKAGE_TARNAME=\"-mccode_name-\" -DPACKAGE_VERSION=\"@MCCODE_VERSION@\" -DPACKAGE_STRING=\"@MCCODE_NAME@\ @MCCODE_VERSION@\" -DPACKAGE_BUGREPORT=\"@MCCODE_TARNAME@-support@@MCCODE_TARNAME@.org\" -DPACKAGE_URL=\"\" -DMCCODE_DATE=\"May.\ 29,\ 2012\" -DMCCODE_TARNAME=\"mcstas\" -DMCCODE_NAME=\"McStas\" -DMCCODE_VERSION=\"May-29-2012\" -DMCCODE_STRING=\"McStas\ May-29-2012\ -\ May.\ 29,\ 2012\" -DMCCODE_BUGREPORT=\"mcstas-support@mcstas.org\" -DMCCODE_PARTICULE=neutron -DMCCODE_LIBENV=MCSTAS -DMCCODE_PROJECT=1 -DMCCODE_PREFIX=mc -DCC_HAS_PROTOS=1 -DSTDC_HEADERS=1 -DHAVE_SYS_TYPES_H=1 -DHAVE_SYS_STAT_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 -DHAVE_MEMORY_H=1 -DHAVE_STRINGS_H=1 -DHAVE_INTTYPES_H=1 -DHAVE_STDINT_H=1 -DHAVE_UNISTD_H=1 -DHAVE_STDLIB_H=1 -DHAVE_MALLOC=1 -DHAVE_STDLIB_H=1 -DHAVE_REALLOC=1 -DHAVE_STRCASECMP=1 -DHAVE_FDOPEN=1 -DHAVE_QSORT=1 -DHAVE_STRCASESTR=1 -DHAVE_NEXUS=/\*\*/ $(DEBUG)
LIBS = 

PERL = /usr/bin/perl

FLEX = flex
FLEXFLAGS=-i

BISON = bison
BISONFLAGS = -v -d

SCILAB = /usr/bin/scilab
MATLAB = no
PGPLOT = /usr/bin/pgxwin_server
VRML = /usr/bin/lookat

WGET = @WGET@

XTERM = @TERM@

INSTALL=/usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644


#
# End of configuration section.
#

all: mcstas

mcstas: $(OBJECTS)
	cd src/ && $(MAKE)

mcstas.win32: $(OBJECTS)
	cd src/ && $(MAKE) mcstas.win32

clean:
	cd src/ && $(MAKE) clean
	rm -f src/config.cache config.cache

distclean:
	cd src/ && $(MAKE) distclean
	rm -f src/config.cache src/Makefile config.cache Makefile

install: 
	cd src/ && $(MAKE) install

# Prefer Scilab over Matlab over PGPLOT
config: plotter
plotter:
	if [ $(VRML) != no ]; then \
		$(MAKE) vrml; \
	fi; \
	if [ $(SCILAB) != no ]; then \
		$(MAKE) scilab; \
	fi; \
	if [ $(MATLAB) != no ]; then \
		$(MAKE) matlab; \
	fi; \
	if [ $(PGPLOT) != no ]; then \
		$(MAKE) pgplot; \
	fi;

scilab:
	sed "s/PLOTTER => '.*'./PLOTTER => 'Scilab'\,/" lib/tools/perl/mccode_config.perl > lib/tools/perl/mccode_config.perl.tmp
	mv lib/tools/perl/mccode_config.perl.tmp lib/tools/perl/mccode_config.perl
matlab:
	sed "s/PLOTTER => '.*'./PLOTTER => 'Matlab'\,/" lib/tools/perl/mccode_config.perl > lib/tools/perl/mccode_config.perl.tmp
	mv lib/tools/perl/mccode_config.perl.tmp lib/tools/perl/mcstas_config.perl
pgplot:
	sed "s/PLOTTER => '.*'./PLOTTER => 'McStas'\,/" lib/tools/perl/mccode_config.perl > lib/tools/perl/mccode_config.perl.tmp
	mv lib/tools/perl/mccode_config.perl.tmp lib/tools/perl/mccode_config.perl
vrml:
	sed "s/PLOTTER => '.*'./PLOTTER => 'VRML'\,/" lib/tools/perl/mccode_config.perl > lib/tools/perl/mccode_config.perl.tmp
	mv lib/tools/perl/mccode_config.perl.tmp lib/tools/perl/mccode_config.perl
PGPLOT: pgplot
Matlab: matlab
Scilab: scilab
VRML: vrml

test:
	cd src && $(MAKE) test

uninstall:
	cd src && $(MAKE) uninstall

reconfigure:
	cd $(libdir_mcstas)/tools/perl/
	$(libdir_mcstas)/tools/perl/mccode_reconfigure

# Optional auto install of patched perl-Tk, scilab and pgplot5 libs
install-pgplot:
	cd support/common && $(MAKE) install-pgplot
	cd support/common && $(MAKE) perl-PGPLOT
	make pgplot
	@echo "McStas: pgplot installed, run 'make reconfigure'"

install-scilab:
	cd support/common && $(MAKE) compile-scilab && echo "McStas: run './configure; make; make install' again"

install-apps:
	if [ -d /usr/share/applications/ ]; then \
	  cp support/common/Desktop/McStas.desktop /usr/share/applications/; \
	fi;
	if [ -d /usr/share/pixmaps/ ]; then \
	  cp support/common/Desktop/mcstas-icon.png /usr/share/pixmaps/; \
	fi;
	if [ -d /usr/share/gtksourceview-1.0/ ]; then \
	  cp support/common/editors/mcstas1.lang /usr/share/gtksourceview-1.0/language-specs/; \
	fi;
	if [ -d /usr/share/gtksourceview-2.0/ ]; then \
	  cp support/common/editors/mcstas2.lang /usr/share/gtksourceview-2.0/language-specs/; \
	fi;
	if [ -d /usr/share/apps/katepart/syntax/ ]; then \
	  cp support/common/editors/mcstas.xml /usr/share/apps/katepart/syntax/; \
	fi;



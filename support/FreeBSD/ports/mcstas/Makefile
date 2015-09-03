# Created by: Erik B Knudsen <erkn@fysik.dtu.dk>
# $FreeBSD$

PORTNAME=	mcstas
PORTVERSION=	2.2a

CATEGORIES=	science
MASTER_SITES=	http://download.mcstas.org/${PORTNAME}-${PORTVERSION}/unix/McStas-${PORTVERSION}-UNIX-src/
DISTNAME=	mcstas-${PORTVERSION}-src

MAINTAINER= erkn@fysik.dtu.dk
COMMENT= Monte Carlo neutron ray tracing package

LICENSE= GPLv2

USES= cmake gmake bison:build

#should perhaps use the toolchain file - but remember the arch
CMAKE_ARGS= "-Denable_mcstas=1"

#GNU_CONFIGURE= yes
#USE_GMAKE= yes
#USE_BISON= yes

.include <bsd.port.mk>


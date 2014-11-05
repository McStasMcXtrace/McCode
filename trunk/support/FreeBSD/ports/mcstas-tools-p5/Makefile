# Created by: Erik B Knudsen
# $FreeBSD$

PORTNAME= mcstas-tools-p5
PORTVERSION= 2.1
#PORTREVISION= beta

#a helper variable
METANAME= McStas-${PORTVERSION}-UNIX-src

CATEGORIES= science
MASTER_SITES= http://download.mcstas.org/mcstas-${PORTVERSION}/unix/
DISTNAME= mcstas-tools-perl-${PORTVERSION}-src
DISTFILES= McStas-${PORTVERSION}-UNIX-src.tar.gz
DIST_SUBDIR= mcstas
EXTRACT_ONLY= ${DISTNAME}${EXTRACT_SUFX}


MAINTAINER= erkn@fysik.dtu.dk
COMMENT= Component Library for the McXtrace Monte Carlo X-ray tracing package

LICENSE= GPLv2

USES= cmake gmake perl5

# Apparently cmake does not generate an install/strip target if nothing
# is supposed to be stripped, so override the cmake-default install
# target of install/strip by resetting STRIP
STRIP= #none

CMAKE_ARGS= "-Denable_mcstas=1"

GNU_CONFIGURE= yes
#USE_GMAKE= yes
#USE_BISON= yes

RUN_DEPENDS= ${LOCALBASE}/mcstas/${PORTVERSION}/bin/mcstas:${PORTSDIR}/science/mcstas\
 ${LOCALBASE}/mcstas/${PORTVERSION}/examples:${PORTSDIR}/science/mcstas-comps\
 p5-Tk>=804:${PORTSDIR}/x11-toolkits/p5-Tk\
 PDL>0:${PORTSDIR}/math/PDL\
 p5-PGPLOT>=2.21:${PORTSDIR}/graphics/p5-PGPLOT

.include <bsd.port.pre.mk>

#this is necessary since mcxtrace and mcstas are packaged in a single tarball in which the
#kernel, component library, and tool tarballs reside
post-fetch:
.if !exists(${DISTDIR}/${DIST_SUBDIR}/${DISTNAME}${EXTRACT_SUFX})
	cd ${DISTDIR}/${DIST_SUBDIR};\
	${TAR} --strip-components 1 -xzf ${DISTFILES} McStas-${PORTVERSION}-UNIX-src/${DISTNAME}.tar.gz;
#	${TAR} -xzf ${DISTFILES} mcxtrace-1.2beta-src.tar.gz;
.endif

.include <bsd.port.post.mk>

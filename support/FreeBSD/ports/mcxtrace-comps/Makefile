# Created by: Erik B Knudsen
# $FreeBSD$

PORTNAME= mcxtrace-comps
PORTVERSION= 1.2beta
#PORTREVISION= beta
CATEGORIES= science
MASTER_SITES= http://download.mcxtrace.org/${PORTVERSION}/
DISTNAME= ${PORTNAME}-${PORTVERSION}-src
DISTFILES= mcxtrace-${PORTVERSION}-unix-src.tgz
DIST_SUBDIR= mcxtrace
EXTRACT_ONLY= ${DISTNAME}${EXTRACT_SUFX}

MAINTAINER= erkn@fysik.dtu.dk
COMMENT= Component Library for the McXtrace Monte Carlo X-ray tracing package

LICENSE= GPLv2

USES= cmake gmake bison:build

# Apparently cmake does not generate an install/strip target if nothing
# is supposed to be stripped, so override the cmake-default install
# target of install/strip by resetting STRIP
STRIP= #none

CMAKE_ARGS= "-DBUILD_MCXTRACE=1"

GNU_CONFIGURE= yes
#USE_GMAKE= yes
#USE_BISON= yes

RUN_DEPENDS= ${LOCALBASE}/mcxtrace/${PORTVERSION}/bin/mcxtrace:${PORTSDIR}/science/mcxtrace

.include <bsd.port.pre.mk>

#this is necessary since mcxtrace and mcstas are packaged in a single tarball in which the
#kernel, component library, and tool tarballs reside
post-fetch:
.if !exists(${DISTDIR}/${DIST_SUBDIR}/${DISTNAME}${EXTRACT_SUFX})
	cd ${DISTDIR}/${DIST_SUBDIR};\
	${TAR} -xzf ${DISTFILES} ${DISTNAME}${EXTRACT_SUFX};
.endif

.include <bsd.port.post.mk>

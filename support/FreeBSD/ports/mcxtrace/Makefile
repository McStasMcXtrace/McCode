# Created by: Erik B Knudsen <erkn@fysik.dtu.dk>
# $FreeBSD$

PORTNAME= mcxtrace
PORTVERSION= 1.2beta
#PORTREVISION= beta
CATEGORIES= science
MASTER_SITES= http://download.mcxtrace.org/${PORTVERSION}/
DISTNAME= ${PORTNAME}-${PORTVERSION}-src
DISTFILES= ${PORTNAME}-${PORTVERSION}-unix-src.tgz
DIST_SUBDIR= ${PORTNAME}
EXTRACT_ONLY= ${DISTNAME}${EXTRACT_SUFX}

MAINTAINER= erkn@fysik.dtu.dk
COMMENT= Monte Carlo X-ray tracing package

LICENSE= GPLv2

USES= cmake gmake bison:build

#should perhaps use the toolchain file - but remember the arch
CMAKE_ARGS= "-DBUILD_MCXTRACE=1"

GNU_CONFIGURE= yes
#USE_GMAKE= yes
USE_BISON= yes

.include <bsd.port.pre.mk>

#this is necessary since mcxtrace and mcstas are packaged in a single tarball in which the
#kernel, component library, manual, and tool tarballs reside
post-fetch:
.if !exists(${DISTDIR}/${DIST_SUBDIR}/${DISTNAME}${EXTRACT_SUFX})
	cd ${DISTDIR}/${DIST_SUBDIR};\
	${TAR} -xzf ${DISTFILES} ${DISTNAME}.tar.gz;
.endif

.include <bsd.port.post.mk>

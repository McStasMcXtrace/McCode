# Created by: Erik B Knudsen
# $FreeBSD$

PORTNAME=	mcstas-comps
PORTVERSION=	2.2a

CATEGORIES=	science
MASTER_SITES=	http://download.mcstas.org/mcstas-${PORTVERSION}/unix/McStas-${PORTVERSION}-UNIX-src/
DISTNAME=	mcstas-comps-${PORTVERSION}-src

MAINTAINER= 	erkn@fysik.dtu.dk
COMMENT= 	Component Library for the McStas Monte Carlo neutron ray tracing package

LICENSE=	GPLv2

USES= 		cmake gmake

# Apparently cmake does not generate an install/strip target if nothing
# is supposed to be stripped, so override the cmake-default install
# target of install/strip by resetting STRIP
STRIP= 		#none

CMAKE_ARGS=	"-Denable_mcstas=1"

RUN_DEPENDS= 	${LOCALBASE}/mcstas/${PORTVERSION}/bin/mcstas:${PORTSDIR}/science/mcstas

.include <bsd.port.mk>

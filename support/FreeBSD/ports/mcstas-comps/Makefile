# Created by: Erik B Knudsen
# $FreeBSD: head/science/mcstas-comps/Makefile 401313 2015-11-12 02:29:59Z junovitch $

PORTNAME=	mcstas-comps
PORTVERSION=	2.2a
CATEGORIES=	science
MASTER_SITES=	http://download.mcstas.org/mcstas-${PORTVERSION}/unix/McStas-${PORTVERSION}-UNIX-src/
DISTNAME=	mcstas-comps-${PORTVERSION}-src

MAINTAINER=	erkn@fysik.dtu.dk
COMMENT=	Component Library for the McStas neutron ray tracing package

LICENSE=	GPLv2

RUN_DEPENDS=	${LOCALBASE}/mcstas/${PORTVERSION}/bin/mcstas:${PORTSDIR}/science/mcstas

USES=		cmake gmake
# Apparently cmake does not generate an install/strip target if nothing
# is supposed to be stripped, so override the cmake-default install
# target of install/strip by resetting STRIP
STRIP=		#none
CMAKE_ARGS=	"-DBUILD_MCSTAS=1"
PLIST_SUB+=	PORTVERSION=${PORTVERSION}

.include <bsd.port.mk>

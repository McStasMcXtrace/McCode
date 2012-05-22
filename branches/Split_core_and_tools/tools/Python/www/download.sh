#!/bin/sh


python_setup() {
    python setup.py build
}


ensure() {
    NAME=$1
    FUNC=$2
    shift; shift;

    echo "Getting: ${NAME}"
    if [ -L ${NAME} ]; then
        echo "* already exists, skipping."
    else
        ${FUNC} ${NAME} $*
    fi

    echo ""
}

cdmake() {
    DIR=$1
    MAKE=$2
    echo ""
    echo "make >>"
    cd ${DIR} && ${MAKE} && cd .. || exit 1
    echo "<< done"
    echo ""
}

build_wget() {
    LINK=$1
    DIR=$2
    RESULT=$3
    MAKE=$4
    URL=$5

    # fetch and unpack
    if [ -d ${DIR} ]; then
        echo "* using existing sources";
    else
        FILE=${DIR}.tar
        wget -O ${FILE} ${URL} &&
        tar xvf ${FILE} &&
        rm -f ${FILE} || exit 1
    fi

    # build and link
    cdmake ${DIR} ${MAKE}
    ln -s ${DIR}/${RESULT} ${LINK} || exit 1
}

# uwsgi
UWSGI=uwsgi-1.2
ensure "uwsgi" \
    build_wget \
    ${UWSGI} \
    "." \
    make \
    "http://projects.unbit.it/downloads/${UWSGI}.tar.gz"

# Flask
FLASK_COMMIT="d5e10e4"
ensure "flask" \
    build_wget \
    "mitsuhiko-flask-${FLASK_COMMIT}" \
    "flask" \
    python_setup \
    "https://github.com/mitsuhiko/flask/tarball/${FLASK_COMMIT}"

# SQLAlchemy
SQLA_VER="0_7"
ensure "sqlalchemy" \
    build_wget \
    "sqlalchemy-rel_${SQLA_VER}" \
    "build/lib*/sqlalchemy" \
    python_setup \
    "http://hg.sqlalchemy.org/sqlalchemy/archive/rel_${SQLA_VER}.tar.gz"


# Flask-SQLAlchemy
FLASK_SQLA_COMMIT="087c3c0"
ensure "flask_sqlalchemy.py" \
    build_wget \
    "mitsuhiko-flask-sqlalchemy-${FLASK_SQLA_COMMIT}" \
    "flask_sqlalchemy.py" \
    python_setup \
    "https://github.com/mitsuhiko/flask-sqlalchemy/tarball/${FLASK_SQLA_COMMIT}"

# Flask-Cache
FLASK_CACHE_COMMIT="568be70"
ensure "flaskext_cache" \
    build_wget \
    "thadeusb-flask-cache-${FLASK_CACHE_COMMIT}" \
    "flaskext" \
    python_setup \
    "https://github.com/thadeusb/flask-cache/tarball/${FLASK_CACHE_COMMIT}"

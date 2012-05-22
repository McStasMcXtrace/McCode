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
        FILE="`basename ${URL}`"
        wget ${URL} &&
        tar xvf ${FILE} &&
        rm -f ${FILE} || exit 1
    fi

    # build and link
    cdmake ${DIR} ${MAKE}
    ln -s ${DIR}/${RESULT} ${LINK} || exit 1
}

build_git() {
    LINK=$1
    DIR=$2
    RESULT=$3
    MAKE=$4
    URL=$5

    # checkout repo
    if [ -d ${DIR} ]; then
        echo "* using existing sources"
    else
        git clone ${URL} || exit 1
    fi

    # build and update symbolic link
    cdmake ${DIR} ${MAKE}
    ln -s ${DIR}/${RESULT} ${LINK}
}


# uwsgi
UWSGI=uwsgi-1.2
ensure "uwsgi" \
    build_wget \
    ${UWSGI} \
    "." \
    make \
    "http://projects.unbit.it/downloads/${UWSGI}.tar.gz"

# Flask-SQLAlchemy
ensure "flask_sqlalchemy.py" \
    build_git \
    "flask-sqlalchemy" \
    "flask_sqlalchemy.py" \
    python_setup \
    "https://github.com/mitsuhiko/flask-sqlalchemy.git"

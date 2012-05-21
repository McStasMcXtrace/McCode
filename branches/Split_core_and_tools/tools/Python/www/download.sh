#!/bin/zsh

function ensure() {
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

function cdmake() {
    DIR=$1
    echo ""
    echo "make >>"
    cd ${DIR} && make && cd .. || exit 1
    echo "<< done"
    echo ""
}

function build_wget() {
    LINK=$1
    DIR=$2
    RESULT=$3
    URL=$4

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
    cdmake ${DIR}
    ln -s ${DIR}/${RESULT} ${LINK} || exit 1
}

function build_git() {
    LINK=$1
    DIR=$2
    RESULT=$3
    URL=$4

    # checkout repo
    if [ -d ${DIR} ]; then
        echo "* using existing sources"
    else
        git clone ${URL} || exit 1
    fi

    # build and update symbolic link
    cdmake ${DIR}
    ln -s ${DIR}/${RESULT} ${LINK}
}


# uwsgi
UWSGI=uwsgi-1.2
ensure "uwsgi" \
    build_wget \
    ${UWSGI} \
    "." \
    "http://projects.unbit.it/downloads/${UWSGI}.tar.gz"

# Flask-SQLAlchemy
ensure "flask_sqlalchemy.py" \
    build_git \
    "flask-sqlalchemy" \
    "flask_sqlalchemy.py" \
    "https://github.com/mitsuhiko/flask-sqlalchemy.git"

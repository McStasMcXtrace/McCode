#!/bin/sh

BUILD=true;
if [ "$1" = "--no-build" ]; then
    BUILD=false;
fi

python_setup() {
    python setup.py build
}


ensure() {
    NAME=$1
    FUNC=$2
    shift; shift;

    echo "Getting: ${NAME}"
    ${FUNC} ${NAME} $*

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

    # build in lib folder
    mkdir -p lib
    cd lib

    # fetch and unpack
    if [ -d ${DIR} ]; then
        echo "* using existing sources";
    else
        FILE=${DIR}.tar
        wget -O ${FILE} ${URL} &&
        tar xvf ${FILE} &&
        rm -f ${FILE} || exit 1
    fi

    if ${BUILD} && [ ! -f ${DIR}/.mcstas-build ]; then
        # build and link
        cdmake ${DIR} ${MAKE}
        touch ${DIR}/.mcstas-build
    fi
    # link from source folder
    cd ..
    rm -f ${LINK}
    ln -s lib/${DIR}/${RESULT} ${LINK} || exit 1
}

# uwsgi
UWSGI=uwsgi-1.4.2
ensure "uwsgi" \
    build_wget \
    ${UWSGI} \
    "." \
    make \
    "http://projects.unbit.it/downloads/${UWSGI}.tar.gz"

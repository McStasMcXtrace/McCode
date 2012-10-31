#!/bin/sh

function usage() {
    echo "usage: $0 name [version] [source]";
    echo "";
    echo "  name:      The name of the distribution (e.g. mcstas)";
    echo "  version:   The version (default: current date)";
    echo "  source:    The source directory (default: name)";
}

if [ "x$1" = "x" ]; then
    # No name specified
    usage;
    exit 1;
fi

# Collect date information
MONTH=`date +"%b"`
DAY=`date +"%d"`
YEAR=`date +"%Y"`

# Set version
if [ "x$2" = "x" ]; then
    MCCODE_VERSION="${YEAR}-${MONTH}-${DAY}";
else
    MCCODE_VERSION="$2";
fi

# Set source
if [ "x$3" = "x" ]; then
    SOURCE="${NAME}";
else
    if [ -d "$3" ]; then
        SOURCE="$3";
    else
        echo "Error: no such directory: $3"
        exit 1;
    fi
fi


# Setup global versioning
NAME="$1"

FLAVOR="${NAME}"
MCCODE_TARNAME="${NAME}"

MCCODE_DATE="${MONTH}. ${DAY}, ${YEAR}";
MCCODE_STRING="${MCCODE_NAME} ${MCCODE_VERSION} - ${MCCODE_DATE}";

case "${NAME}" in
    "mcstas" )
        export MCCODE_NAME="McStas";
        ;;
    "mcxtrace" )
        export MCCODE_NAME="McXtrace";
        ;;
esac


# Constants
DIST=dist


function config_mccode() {
    for file in $(find . -name "CMakeLists.txt" -or -name "*.in"); do
        echo ${file}
        for i in 1 2 3 4 5; do
            # replace variables into file.tmp
            sed -e 's/@MCCODE_NAME@/'"${MCCODE_NAME}"'/' \
                -e 's/@MCCODE_TARNAME@/'"${MCCODE_TARNAME}"'/' \
                -e 's/@MCCODE_VERSION@/'"${MCCODE_VERSION}"'/' \
                -e 's/@MCCODE_DATE@/'"${MCCODE_DATE}"'/' \
                -e 's/@MCCODE_STRING@/'"${MCCODE_STRING}"'/' \
                "${file}" > "${file}.tmp"
            # rename "fixed" version to file
            mv ${file}.tmp ${file};
        done;
    done;
}


function remove_all() {
    # Recursively REMOVE everything matched by glob
    while true; do
        # Next pattern
        P="$1"
        if [ "x${P}" = "x" ]; then
            break;
        fi
        # Remove files
        find . -name "${P}" -exec rm -vrf {} \;
        # Move to next argument
        shift;
    done
}


function cleanup() {
    # if there's a makefile, try "make clean"
    if [ -f Makefile ]; then
        make clean
    fi

    # automake and CMake directories
    rm -rf autom*.cache       \
        build work CMakeFiles

    # CMake files
    rm -f CMakeCache.txt cmake_install.cmake       \
        CPackSourceConfig.cmake CPackConfig.cmake

    # backup files
    remove_all              \
        "#*#"               \
        ".git" ".svn"       \
        "*.o"               \
        "lex.yy.c"          \
        "instrument.tab.?"

    # remove generated files with a corresponding *.in file
    for f in $(find . -iname "*.in"); do
        genf=$(echo $f | rev | cut -f2-100 -d'.' | rev);
        rm -f ${genf};
    done
}

function prepare_mccode() {
    config_mccode;
    autoconf;

    (
        cd src;
        flex instrument.l;
        bison instrument.y;
    )
}

function build_mccode() {
    prepare_mccode;
    ./configure $1 &&
    make
}


function make_source() {
    SOURCE=$1

    OUT=${DIST}/${SOURCE}-src;

    cp -Lr ${SOURCE} ${OUT};

    (
        cd ${OUT};
        cleanup &&
        prepare_mccode;
    )
}

function make_bin() {
    NAME=$1
    SOURCE=$2

    if [ "x${SOURCE}" = "x" ]; then
        SOURCE="${NAME}";
    fi

    OUT=${DIST}/${NAME}-bin;

    cp -Lr ${SOURCE} ${OUT};

    (
        cd ${OUT};
        cleanup &&
        build_mccode "--prefix=`pwd`/build" &&
        make install;
        (
            cd build;
            tar zcf ../${NAME}.tar.gz *;
        )
    )
}


function make_deb() {
    SOURCE=$1

    OUT=${DIST}/${SOURCE}-deb;

    cp -Lr ${SOURCE} ${OUT};

    (
        cd ${OUT};
        cleanup &&
        cmake . &&
        cpack -G DEB
    )
}


# clean up
rm -r ${DIST}

mkdir -p ${DIST}

# copy needed files to dist
cp mkinstalldirs ${DIST}

# make_source "mcstas"
# make_source "mcxtrace"

make_bin "${NAME}" "${SOURCE}"
# make_bin "mcxtrace"

# make_deb "mcstas"
# make_deb "mcxtrace"

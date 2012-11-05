#!/bin/sh

if [ "x${MCINSTALL_PREFIX}" = "x" ]; then
    MCINSTALL_PREFIX="/usr/local";
fi
export MCINSTALL_PREFIX


function usage() {
    echo "usage: $0 name [version] [source] [target] [-- gens ...] ";
    echo "";
    echo "  name:      The name of the distribution (e.g. mcstas)";
    echo "  version:   The version (default: current date)";
    echo "  source:    The source directory (default: name)";
    echo "  target:    The target directory (default: dist)";
    echo "  gens:      Generators to use (src, bin, deb, rpm)";
    echo ""
    echo "McStas, Johan Brinch <jsbn@fysik.dtu.dk>";
}

if [ "x$1" = "x" ]; then
    # No arguments
    usage;
    exit 1;
fi


# Parse arguments into variables; stop at --
args="NAME MCCODE_VERSION SOURCE DEST"
for a in ${args}; do
    VAL="$1";

    # Break on EOF or --
    if [ "x${VAL}" = "x--" ]; then
        shift;
        break;
    fi

    # Set argument from args and shift to next
    export ${a}="$1";
    shift;
done

# Any arguments from here on are generators (e.g. src, bin, deb or rpm)
GENS="$*"

# Set default generators
if [ "x${GENS}" = "x" ]; then
    GENS="src bin";
fi


# Set environment constants
TOP="`pwd`"


# Set name-specific constants
case ${NAME} in
    mctools )
        CONFIGURE_FLAGS="-Denable_mcstas=1";
        ;;
    mxtools )
        CONFIGURE_FLAGS="-Denable_mcxtrace=1";
        ;;
esac


# Convert any path to an absolute path
function get_absolute() {
    (
        cd $(dirname "$1");
        echo `pwd`/$(basename "$1");
    )
}


# Collect date information
MONTH=`date +"%b"`
DAY=`date +"%d"`
YEAR=`date +"%Y"`

# Set default version
if [ "x${MCCODE_VERSION}" = "x" ]; then
    MCCODE_VERSION="${YEAR}-${MONTH}-${DAY}";
fi


# Set source directory
if [ "x${SOURCE}" = "x" ]; then
    SOURCE="`pwd`/${NAME}";
fi

# Make absolute
if [ -d "${SOURCE}" ]; then
    SOURCE="`get_absolute "${SOURCE}"`";
else
    echo "Error: source directory not found: ${SOURCE}"
    exit 1;
fi


# Set default distination directory
if [ "x${DEST}" = "x" ]; then
    DEST="${TOP}/dist";
else
    # Make absolute
    DEST="`get_absolute "${DEST}"`";
fi

if [ -d "${DEST}/.git" ] || [ -d "${DEST}/.svn" ]; then
    echo "Error: distination dir looks like a source repository: ${DEST}";
    echo "> Refusing to delete it!"
    exit 1;
fi


# Setup global versioning
if [ "x${FLAVOR}" = "x" ]; then
    if ( echo ${NAME} | grep mcx ) || ( echo ${NAME} | grep mx ) then
        FLAVOR="mcxtrace"
    else
        FLAVOR="mcstas"
    fi
fi

echo ""
echo "=== Picked flavor: ${FLAVOR}"
echo ""


MCCODE_TARNAME="${FLAVOR}"
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



function config_mccode() {
    find . -type f                 \
        -name "CMakeLists.txt" -or \
        -name "*.cmake"        -or \
        -name "*.in" |
    while read file; do
        for i in 1 2 3 4 5; do
            # replace variables into file.tmp
            sed -e 's/@MCCODE_NAME@/'"${MCCODE_NAME}"'/' \
                -e 's/@MCCODE_TARNAME@/'"${MCCODE_TARNAME}"'/' \
                -e 's/@MCCODE_VERSION@/'"${MCCODE_VERSION}"'/' \
                -e 's/@MCCODE_DATE@/'"${MCCODE_DATE}"'/' \
                -e 's/@MCCODE_STRING@/'"${MCCODE_STRING}"'/' \
                "${file}" > "${file}.tmp"
            # rename "fixed" version to file
            mv "${file}.tmp" "${file}";
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
        rm -f "${genf}";
    done
}

function prepare_mccode() {
    config_mccode;
    autoconf;

    if [ -f src/instrument.l ]; then
        (
            cd src &&
            flex instrument.l  &&
            bison instrument.y
        )
    fi
}

function build_mccode() {
    # prepare_mccode;
    # ./configure $1 &&
    # make
    mkdir build && cd build;
    cmake $1 ..
}


function fresh_copy() {
    rm -rf "${2}" &&
    cp -LR "${1}" "${2}";
}

function fresh_clean_copy() {
    DEST="${1}"
    fresh_copy "${SOURCE}" "${DEST}";
    (
        cd "${DEST}";
        cleanup &&
        config_mccode
    )
}


function make_src() {
    OUT="${DEST}/${NAME}-src";
    fresh_clean_copy "${OUT}";
    (
        cd "${OUT}";
        prepare_mccode;
    )
}

function prepare_cpack() {
    # copy source files
    DEST="${1}"
    fresh_clean_copy "${DEST}";
    (
        cd "${DEST}";
        cmake ${CONFIGURE_FLAGS} .
    )
}

function simple_cpack() {
    # make a CPack package from GEN and destination
    # e.g. simple_cpack DEB "dist/deb"

    GEN="$1"
    DEST="$2"
    prepare_cpack "${DEST}";
    (
        cd "${DEST}" &&
        cpack -G "${GEN}";
    )
}


function make_bin() {
    simple_cpack TGZ "${DEST}/${NAME}-tgz";
}

function make_deb() {
    simple_cpack DEB "${DEST}/${NAME}-deb";
}

function make_rpm() {
    simple_cpack RPM "${DEST}/${NAME}-rpm";
}

function make_OSXpkg() {
    simple_cpack PackageMaker "${DEST}/${NAME}-OSXpkg";
}


# create dist
echo "${SOURCE} -> ${DEST}"
mkdir -p "${DEST}"

# copy needed files to dist
cp mkinstalldirs "${DEST}"

# move into dist
cd "${DEST}" || exit 1


for gen in ${GENS}; do
    echo ""
    echo "=== ${gen} ===";
    echo ""

    # run generator
    make_${gen};

    echo "done."
done

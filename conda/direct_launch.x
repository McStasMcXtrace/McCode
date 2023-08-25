#!/usr/bin/env bash
set -e
set -u
REPOROOT="$( cd -P "$( dirname "${BASH_SOURCE[0]}/" )/.." && pwd )"
CONDARECIPES="${REPOROOT}/conda/recipe"
TMPDIR="${REPOROOT}/conda/direct_launch_tmpdir"
if [ -d "${TMPDIR}" ]; then
    echo "ERROR: Please remove ${TMPDIR} and rerun"
    exit 1
fi
mkdir -p "${TMPDIR}"

for subpkg in core data; do
    cd ${TMPDIR}
    mkdir install-${subpkg}
    mkdir work-${subpkg}
    cd "${TMPDIR}/work-${subpkg}"
    ln -s $REPOROOT src
    PREFIX="${TMPDIR}/install-${subpkg}" CPU_COUNT=0 PYTHON="$(which python3)" python3 $CONDARECIPES/install-files-${subpkg}.py
done

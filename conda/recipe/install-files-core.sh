#!/usr/bin/env bash
set -x
set -e
set -u

mkdir -p build

cd build

cmake \
    -DCMAKE_INSTALL_PREFIX="${PREFIX}" \
    "${SRC_DIR}/src" \
    -DBUILD_SHARED_LIBS=ON \
    -DCMAKE_INSTALL_LIBDIR=lib \
    -DCMAKE_BUILD_TYPE=Release \
    -DBUILD_MCSTAS=ON \
    -DMCCODE_USE_LEGACY_DESTINATIONS=OFF \
    -DBUILD_TOOLS=ON \
    -DENABLE_COMPONENTS=ON \
    -DENSURE_MCPL=OFF \
    -DENSURE_NCRYSTAL=OFF \
    -DENABLE_CIF2HKL=OFF \
    -DENABLE_NEUTRONICS=OFF \
    ${CMAKE_ARGS}

make -j${CPU_COUNT:-1}
make install

test -f "${PREFIX}/bin/mcstas"
test -f "${PREFIX}/bin/mcrun"
test -f "${PREFIX}/share/mcstas/tools/Python/mccodelib/__init__.py"
test -d "${PREFIX}/share/mcstas/resources/data/"

#Data files will be provided in mcstas-data package instead:
rm -rf "${PREFIX}/share/mcstas/resources/data/"

#TEMPORARY WORKAROUNDS:
rm -f  "${PREFIX}/bin/postinst"
mv "${PREFIX}/bin/acc_gpu_bind" "${PREFIX}/bin/mcstas-acc_gpu_bind"

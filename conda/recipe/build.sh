#!/bin/bash
set -x
set -e

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
    -DBUILD_TOOLS=OFF \
    -DENABLE_COMPONENTS=ON \
    -DENSURE_MCPL=OFF \
    -DENSURE_NCRYSTAL=OFF \
    -DENABLE_CIF2HKL=OFF \
    -DENABLE_NEUTRONICS=OFF \
    ${CMAKE_ARGS}

#cmake --build . --config Release
#cmake --install .
make -j${CPU_COUNT:-1}
make install

#NB: Could add some "make test" command here if available.

cd ..

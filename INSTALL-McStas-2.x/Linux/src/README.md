# Installing McStas 2.7 from preconfigured source code

* Download and build the McStas source code for Unix systems:
```bash
wget http://downloads.mcstas.org/current/unix/mcstas-2.7-UNIX-src.tar.gz
(or curl -O or fetch or...)
```
* Unpack the "metapackage" tarball
```bash
tar xzf mcstas-2.7-UNIX-src.tar.gz
cd mcstas-2.7-UNIX-src/
```
* Unpack the individual subpackages
```bash
find . -name \*tar.gz -exec tar xzf \{\} \;
```
* Next, compile the individual packages you want (minimum set is mcstas-2.7-src mcstas-comps-2.7-src) using e.g.
```bash
cd mcstas-2.7-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
```

You will of course have to install needed dependencies along the way, we suggest having a look at BUILD_DEPENDENCIES.TXT and the getdeps_ scripts from the top of the repo for inspiration.

Also, we provide experimental deployment scripts for [FreeBSD 12](fetch_install_mcstas-2.7-freebsd-12.sh) and [TrueOS 18.6](fetch_install_mcstas-2.7-trueos-18.6.sh)

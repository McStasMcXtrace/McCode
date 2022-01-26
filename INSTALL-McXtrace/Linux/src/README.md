# Installing McStas 1.5rc1 from preconfigured source code

* Download and build the McXtrace source code for Unix systems:
```bash
wget http://downloads.mcxtrace.org/current/unix/mcxtrace-1.5rc1-UNIX-src.tar.gz
(or curl -O or fetch or...)
```
* Unpack the "metapackage" tarball
```bash
tar xzf mcxtrace-1.5rc1-UNIX-src.tar.gz
cd mcxtrace-1.5rc1-UNIX-src/
```
* Unpack the individual subpackages
```bash
find . -name \*tar.gz -exec tar xzf \{\} \;
```
* Next, compile the individual packages you want (minimum set is mcxtrace-1.5rc1-src mcxtrace-comps-1.5rc1-src) using e.g.
```bash
cd mcxtrace-1.5rc1-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcxtrace=1
make
sudo make install
```

You will of course have to install needed dependencies along the way, we suggest having a look at BUILD_DEPENDENCIES.TXT and the getdeps_ scripts from the top of the repo for inspiration.


# Installing McXtrace 3.2 from preconfigured source code

* Download and build the McXtrace source code for Unix systems:
```bash
wget https://downloads.mcxtrace.org/mcxtrace-3.2/unix/mcxtrace-3.2-UNIX-src.tar.gz
(or curl -O or fetch or...)
```
* Unpack the "metapackage" tarball
```bash
tar xzf mcxtrace-3.2-UNIX-src.tar.gz
cd mcxtrace-3.2-UNIX-src/
```
* Unpack the individual subpackages
```bash
find . -name \*tar.gz -exec tar xzf \{\} \;
```
* Next, compile the individual packages you want (minimum set is mcxtrace-3.2-src mcxtrace-comps-3.2-src) using e.g.
```bash
cd mcxtrace-3.2-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -DBUILD_MCXTRACE=1
make
sudo make install
```

You will of course have to install needed dependencies along the way, we suggest having a look at BUILD_DEPENDENCIES.TXT and the getdeps_ scripts from the top of the repo for inspiration.


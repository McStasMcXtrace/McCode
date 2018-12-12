# Installing McStas 2.5 from preconfigured source code

* Download and build the McStas source code for Unix systems:
```bash
wget http://downloads.mcstas.org/current/unix/McStas-2.5-UNIX-src.tar.gz
```
* Unpack the "metapackage" tarball
```bash
tar xzf McStas-2.5-UNIX-src.tar.gz
cd McStas-2.5-UNIX-src/
```
* Unpack the individual subpackages
```bash
find . -name \*tar.gz -exec tar xzf \{\} \;
```
* Next, compile the individual packages you want (minimum set is mcstas-2.5-src mcstas-comps-2.5-src) using e.g.
```bash
cd mcstas-2.5-src
cmake -DCMAKE_TOOLCHAIN_FILE=cmake/toolchains/freebsd64.cmake -Denable_mcstas=1
make
sudo make install
```


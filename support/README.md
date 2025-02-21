# This folder contains platform-support snippets of different type
(Consider merge with ../meta-packages)

## Contents:

### [Win32](Win32)
- Bits and pieces for INNOsetup packages otherwise found in
  ../meta-packages. Will be deprecated once cross-compilation from
  Linux is fully abbandoned

### [MacOSX](MacOSX)
- Thin self-injecting  McCode-conda.app structure for macOS, based on conda-forge
  (plus older incarnation - cleanup needed)

### [FreeBSD](FreeBSD)
- (obsolete) Port-oriented scripts for FreeBSD, assume availability of
  "src" preconfigured source-code on mccode.org. Better use
  CMake/CPack with pkg backend.

### [common](common)
- Desktop launcher and editor syntax highlighting profiles

### [git](git)
- a reasonable .gitignore file?


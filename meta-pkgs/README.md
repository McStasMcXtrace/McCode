# This folder contains plumbing for "metapackages" 

## Contents:

### [windows](windows)
- INNOsetup-driven ".exe" installer input for wrapping CPack generated
  packages, cross-compiled from Linux. Will likely become deprecated
  during spring of 2025 when a full Windows infrastructure is in place
  via conda-forge.

### [deb](deb)
- Thin debian-package definitions for McStas and McXtrace "-suite"
  metapackages. (Will these survive transition to official Debian packages??)

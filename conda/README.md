This directory contains recipes for building McStas conda packages.

WARNING: These recipes are work-in-progress and not ready for general usage yet!

A few tips for developers:

* Install conda-build pkg in base env, since it must live next to the conda pkg
  itself (it seems): `conda install -n base conda-build`
* Cleanup build leftovers with `conda build purge` or `conda build purge-all`
  since they can get quite large (use with care as this cannot be reversed).
* Use the libmamba solver for faster dependency solving:
  ```
  conda update -n base conda
  conda install -n base conda-libmamba-solver
  conda config --set solver libmamba
  ```
  Undo last  step with `conda config --set solver classic`
* Update conda with: `conda update -n base -c conda-forge conda`

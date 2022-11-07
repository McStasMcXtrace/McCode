include(fetcher)

set(NCRYSTAL_REPO "https://github.com/mctools/ncrystal.git")
set(NCRYSTAL_MINIMUM_VERSION 3.4.1)

option(NCRYSTAL_REQUIRED "Require pre-installed NCRYSTAL >= ${NCRYSTAL_MINIMUM_VERSION}" OFF)

git_fetch(ncrystal ${NCRYSTAL_MINIMUM_VERSION} ${NCRYSTAL_REPO} ${NCRYSTAL_REQUIRED})

# Now set mcpl properties in the calling scope?

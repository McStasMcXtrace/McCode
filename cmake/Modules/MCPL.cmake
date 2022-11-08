include(fetcher)

set(MCPL_REPO "https://github.com/mctools/mcpl.git")
set(MCPL_MINIMUM_VERSION 1.5.1)

option(MCPL_REQUIRED "Require pre-installed MCPL >= ${MCPL_MINIMUM_VERSION}" OFF)

git_fetch(mcpl ${MCPL_MINIMUM_VERSION} ${MCPL_REPO} ${MCPL_REQUIRED})

# Now set mcpl properties in the calling scope?

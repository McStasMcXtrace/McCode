# This document describes the efforts towards providing McXtrace and McStas as docker images.
In the following:
- FLAVOUR is either 'mcstas' or 'mcxtrace' in small letters unless otherwise noted.
- VERSION is the version of McStas/McXtrace, eg. 2.6.1, or 1.5
- IMAGEVERSION is the version number of the docker image - i.e. not the program version.

# To build a new image:
```docker image build --tag mccode/FLAVOUR-VERSION:IMAGEVERSION .```
The punctuation mark at the end, is necessary to tell docker where to find the
\'Dockerfile\'

# To push the image to dockerhub:
```docker image push mccode/FLAVOUR-VERSION:IMAGEVERSION```

# To run 
## The McXtrace debian based docker
Simply run the script docker_mxgui.sh, this will pull an image from dockerhub and cache it locally.

## The McStas debian based docker
Simply run the script docker_mcgui.sh, this will pull an image from dockerhub and cache it locally.

In both cases this will:
- You will run as the user "docker" inside the docker.
- Currently the pyqtplot utilities do not work - nor does webgl. It apparently *does* work
when run on a Mac OSX host.
- Matplotlib plotting works
- Mounts the users home directory as the home directory for the docker run.

# TODO still
1. Change to use the users username etc instead of docker
2. Use the currrent work dir as work-dir for the docker.
3. Fix the broken connection to plotting tools

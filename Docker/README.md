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
Simply run the script ```docker_mxgui.sh```, this will pull an image from dockerhub and cache it locally.

## The McStas debian based docker
Simply run the script ```docker_mcgui.sh```, this will pull an image from dockerhub and cache it locally.

**In both cases:**
- You will run as the user "docker" inside the docker.
- Mounts the users home directory as the home directory for the docker run.

# On macOS you will also need
- To install Docker Desktop for macOS
- To install XQuartz

# On windows you will also need
- To install Docker Desktop
- To install Xming
- Set your user homedir among the docker shared folders

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

## The McXtrace debian based docker on Linux / macOS:
Simply run the script ```docker_mxgui.sh```, this will pull an image from dockerhub and cache it locally.

## The McXtrace debian based docker on Windows:
Simply run the script ```docker_mxgui.bat```, this will pull an image from dockerhub and cache it locally.

## The McStas debian based docker on Linux /macOS:
Simply run the script ```docker_mcgui.sh```, this will pull an image from dockerhub and cache it locally.

## The McStas debian based docker on Windows:
Simply run the script ```docker_mcgui.bat```, this will pull an image from dockerhub and cache it locally.

**In both cases:**
- You will run as the user "docker" inside the docker.
- Mounts the users home directory as the home directory for the docker run.

# On Linux you will also need
- To [install Docker for your variant of Linux](https://docs.docker.com/engine/install/#server)

# On macOS you will also need
- To install [Docker Desktop for macOS](https://docs.docker.com/docker-for-mac/install/)
- To install [XQuartz](https://www.xquartz.org)
- Use the [XQuartz preferences](https://raw.githubusercontent.com/McStasMcXtrace/McCode/master/Docker/images/XQuartz-prefs.png) pane to ["Allow connections from network clients"](https://raw.githubusercontent.com/McStasMcXtrace/McCode/master/Docker/images/Allow-connections-from-network-clients.png)

# On windows you will also need
- To install [Docker Desktop](https://docs.docker.com/docker-for-windows/install/)
- To install [Xming](https://sourceforge.net/projects/xming/files/latest/download)
- Create the subfolder McXtrace-dockers and / or McStas-dockers  in
  your homedir
- Set your [user homedir/the above folder](https://raw.githubusercontent.com/McStasMcXtrace/McCode/master/Docker/images/Docker-resources-file-sharing.png) among the docker shared folders

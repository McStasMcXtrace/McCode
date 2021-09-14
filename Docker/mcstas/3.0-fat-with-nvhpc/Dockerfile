FROM nvcr.io/nvidia/nvhpc:20.11-devel-cuda11.1-ubuntu20.04
ENV VERSION=3.0
RUN apt-get --yes update && apt-get --yes dist-upgrade
RUN apt-get --yes install tzdata
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install keyboard-configuration
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install coreutils curl xbase-clients xdg-utils firefox libosmesa6 mesa-utils libgl1-mesa-glx emacs vim fonts-liberation cpanminus git cmake
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install libhdf5-103 libhdf5-dev openmpi-bin libopenmpi-dev
RUN curl -O https://codeload.github.com/nexusformat/code/tar.gz/v4.4.3 && tar xzf v4.4.3 && cd code-4.4.3 && mkdir tmp && cd tmp && export CC=gcc && export CXX=g++ && cmake .. && make && make install
RUN curl http://packages.mccode.org/debian/mccode.list > /etc/apt/sources.list.d/mccode.list
RUN apt-get --yes update
RUN apt-get --yes install mcstas-suite-python-ng mcstas-suite-perl-ng
RUN update-alternatives --install /bin/sh sh /bin/bash 200
RUN update-alternatives --install /bin/sh sh /bin/dash 100
RUN cpan install PDL
RUN groupadd docker
RUN useradd -g docker docker
ENV HOME /home/docker
WORKDIR /home/docker

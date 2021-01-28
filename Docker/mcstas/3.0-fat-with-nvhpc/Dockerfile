FROM nvcr.io/nvidia/nvhpc:20.11-devel-cuda11.1-ubuntu20.04
ENV VERSION=3.0
RUN apt-get --yes update && apt-get --yes dist-upgrade
RUN apt-get --yes install tzdata
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install keyboard-configuration
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install coreutils curl xbase-clients xdg-utils firefox libosmesa6 mesa-utils libgl1-mesa-glx openmpi-bin libopenmpi-dev libnexus1 libnexus-dev emacs vim fonts-liberation cpanminus
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

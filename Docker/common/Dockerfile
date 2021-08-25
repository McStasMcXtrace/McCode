FROM fullaxx/ubuntu-desktop:xfce4
ENV VERSION=3.0
RUN apt-get --yes update
RUN apt-get --yes install tzdata
RUN apt-get --yes install keyboard-configuration
RUN apt-get --yes install coreutils curl xbase-clients xdg-utils firefox libosmesa6 mesa-utils libgl1-mesa-glx openmpi-bin libopenmpi-dev emacs vim fonts-liberation
RUN curl http://packages.mccode.org/debian/mccode.list > /etc/apt/sources.list.d/mccode.list
RUN apt-get --yes update
RUN apt-get --yes install mcstas-suite-python
RUN apt-get --yes install mcstas-suite-python-ng
RUN apt-get --yes install mcxtrace-suite-python
RUN apt-get --yes install libnexus-dev libnexus1
RUN update-alternatives --install /bin/sh sh /bin/bash 200
RUN update-alternatives --install /bin/sh sh /bin/dash 100
RUN groupadd docker
RUN useradd -g docker docker
WORKDIR /home/docker

FROM debian:latest
ENV VERSION=1.5
RUN apt-get --yes update && apt-get --yes dist-upgrade
RUN apt-get --yes install coreutils curl xterm
RUN curl http://packages.mccode.org/debian/mccode.list >/etc/apt/sources.list.d/mccode.list
RUN apt-get --yes update
RUN apt-get --yes install mcxtrace-suite-python
RUN groupadd docker
RUN useradd -g docker docker
ENV HOME /home/docker
WORKDIR /home/docker

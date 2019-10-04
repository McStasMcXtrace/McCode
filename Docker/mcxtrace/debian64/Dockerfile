FROM debian:latest
COPY non-free-repo.list /etc/apt/sources.list.d/
RUN apt-get --yes update && apt-get --yes dist-upgrade
ENV VERSION=1.5rc1
WORKDIR /app
COPY dist/mcxtrace-$VERSION-deb64.deb /app
COPY dist/mcxtrace-comps-$VERSION-deb64.deb /app
COPY dist/mcxtrace-tools-perl-cmdline-$VERSION-deb64.deb /app
RUN apt-get -y install git flex bison gcc gfortran build-essential libgsl-dev 
RUN apt-get -y install perl-tk pdl pgplot5 bc libpgplot-perl
RUN git clone --depth 1 --recursive https://github.com/McStasMcXtrace/McCode.git McCode
RUN cd McCode 
RUN git pull
RUN ./build_debs_mcxtrace 1.5 yes
RUN cd dist 
RUN dpkg --install *.deb
CMD ["mxrun.pl","--test"]

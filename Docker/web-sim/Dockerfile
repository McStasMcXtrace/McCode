FROM phusion/baseimage:bionic-1.0.0
# Use baseimage-docker's init system.
CMD ["/sbin/my_init"]
ENV VERSION=2.6.1
RUN apt-get --yes update && apt-get --yes dist-upgrade
RUN apt-get --yes install tzdata
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install coreutils git curl xbase-clients xdg-utils openmpi-bin libopenmpi-dev nano cpanminus
RUN curl http://packages.mccode.org/debian/mccode.list > /etc/apt/sources.list.d/mccode.list
RUN apt-get --yes update
RUN apt-get --yes install mcstas-suite-python mcstas-suite-perl mcxtrace-suite-python mcxtrace-suite-perl mcstas-tools-python-mcplot-svg-2.6.1
RUN apt-get --yes install python-dev libssl-dev makepasswd
RUN curl https://raw.githubusercontent.com/McStasMcXtrace/McCode/master/tools/Python/mcgui/mcgui.py > /usr/share/mcstas/2.6.1/tools/Python/mcgui/mcgui.py
RUN curl https://raw.githubusercontent.com/McStasMcXtrace/McCode/master/tools/Python/mccodelib/mccode_config.py  > /usr/share/mcstas/2.6.1/tools/Python/mccodelib/mccode_config.py
RUN update-alternatives --install /bin/sh sh /bin/bash 200
RUN update-alternatives --install /bin/sh sh /bin/dash 100
RUN cpan install PDL
RUN groupadd docker
RUN useradd -g docker docker
ENV HOME /home/docker
WORKDIR /home/docker
RUN git clone --single-branch --branch slurm-worker-prototype https://github.com/McStasMcXtrace/McWeb.git /home/docker/McWeb
RUN apt-get --yes install python-pip python-ldap libsasl2-dev python-dev libldap2-dev libssl-dev telnet
RUN apt-get --yes install ssh openssh-server
RUN apt-get --yes dist-upgrade
RUN pip install -I Django==1.11.28 django-auth-ldap==1.2.7 simplejson
RUN chown -R docker:docker /home/docker/
RUN cd /home/docker/McWeb/mcsimrunner; cp tests/settings.py mcweb/settings.py
RUN cd /home/docker/McWeb/mcsimrunner; mkdir sim/intro-ns ; cp tests/instrument-files/* sim/intro-ns
RUN cd /home/docker/McWeb/mcsimrunner; sed -i "s+/srv/mcweb/McWeb/mcsimrunner/static/+/home/docker/McWeb/mcsimrunner/static/+g" mcweb/settings.py
RUN cd /home/docker/McWeb/mcsimrunner; sed -i "s/SANSsimple/templateSANS2/g" mcweb/settings.py
RUN cd /home/docker/McWeb/mcsimrunner; sed -i "s/DEBUG\ =\ False/DEBUG\ =\ True/g" mcweb/settings.py
RUN cd /home/docker/McWeb/mcsimrunner; sed -i "s/STATIC_ROOT/\#STATIC_ROOT/g" mcweb/settings.py
RUN cd /home/docker/McWeb/mcsimrunner; sed -i "s/svg-dev/svg/g" mcweb/settings.py
RUN cd /home/docker/McWeb/mcsimrunner; python manage.py migrate
RUN cd /home/docker/McWeb/mcsimrunner; python manage.py collect_instr
RUN cd /home/docker/McWeb/mcsimrunner; echo "from django.contrib.auth import get_user_model; User = get_user_model(); User.objects.create_superuser('docker', 'admin@localhost', 'docker')" | python manage.py shell
RUN mkdir -p /srv/mcweb && ln -sf /home/docker/McWeb/ /srv/mcweb/McWeb
RUN chown -R docker:docker /home/docker/
RUN cd /home/docker/McWeb/mcsimrunner; curl https://raw.githubusercontent.com/McStasMcXtrace/McCode/master/Docker/web-sim/go.sh > go.sh
RUN cd /home/docker/McWeb/mcsimrunner; cat go.sh
RUN cd /home/docker/McWeb/mcsimrunner; chmod a+x go.sh
# Clean up APT when done.
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
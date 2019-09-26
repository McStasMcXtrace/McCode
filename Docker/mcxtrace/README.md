# How to run the McXtrace debian based mxrun.pl --test docker
1. Make sure you have .deb-files in the dist folder
1. Copy distribution files to a local dist directory (Apparently links don't work)
> cp -r ../../dist .
1. Build your docker image using e.g.
> [sudo] docker image build --tag [whatever]/mcxtraceTest:1.0
2. Run it:
> [sudo] docker container run [whatever]/mcxtraceTest:1.0

## TODO still
1. publish the docker image to some publicly accesssible site
1. add interactive abilities to enable runnig McXtrace "for real" with arbitrary instruments.

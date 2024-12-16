# Running McStas and McXtrace 3.5.16 in Docker / podman etc.

## Pending update!

The container definition is available at [here](https://github.com/willend/jupyter-remote-desktop-proxy/tree/mcstas-mcxtrace-3.5) and is further on dockerhub under docker.io/mccode/mcstas-mcxtrace

To run the container locally, use e.g.
```podman run -p 8888:8888 -p 5173:5173 docker.io/mccode/mcstas-mcxtrace```

A corresponding container is also availalble on binder: [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/McStasMcXtrace/jupyter-mcstas-mcxtrace-desktop/main?urlpath=desktop)

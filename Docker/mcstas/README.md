# How to run the McStas debian based docker
Simply run the script docker_mcgui
- You will run as the user "docker" inside the docker.
- Currently the pyqtplot utilities do not work - nor does webgl.
- Matplotlib plotting works
- Mounts the users home directory as the home directory for the docker run.

# On windows you will also need
- To install Docker Desktop
- To install Xming
- Set your user homedir among the docker shared folders

## TODO still
1. Change to use the users username etc instead of docker
2. Use the currrent work dir as work-dir for the docker.
3. Fix the broken connection to plotting tools

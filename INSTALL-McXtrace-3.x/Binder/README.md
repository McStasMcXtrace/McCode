# Running McXtrace directly in browser - No Installation !


[McXtrace](https://mcxtrace.org/) X-ray beam-line simulations in a "free" computer at Binder.

<img src="https://mcxtrace.org/mcxtrace_1.png"> <img src="https://mybinder.org/static/logo.svg?v=fe52c40adc69454ba7536393f76ebd715e5fb75f5feafe16a27c47483eabf3.21c14ed9fda905c49915d6dbf369ae68fb855a40dd05489a7b9542a9ee532e92b">

McXtrace is available on Binder, which provides a free ready-to-run environment for simulations in your internet browser.

Both the legacy version 1.x and the 'next-generation' 3.x (with revised grammar, compilation and performance efficiency) are available. You are welcome to use MPI clustering to distribute the computations over all CPU cores. There is however no GPU support for the 3.x release through Binder.

:warning: The Binder service is free, but provides a limited computing power. This is suited for training and reasonable simulations.

## Usage

Start it in a single click here:  [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/McStasMcXtrace/mcxtrace-binder/master?urlpath=desktop)

A full desktop, running at Binder for free, will appear in your browser after e.g. a few minutes (be patient).
The [McXtrace](https://mcxtrace.org/) software is available from the __Applications__ menu in group __Education__.

<img src="https://mcxtrace.org/files/mcxtrace-binder.png">

## Usage: JupyterLab to exchange files with the running environment

You may as well start the JupyterLab environment, which allows to exchange files with the running McXtrace environment. 

- The right panel provides a *Desktop* item to start the environment.
- The left panel allows files drag-n-drop between your own computer and the running Binder session (esp. the *work* directory).

Start it in a single click here:  [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/McStasMcXtrace/mcxtrace-binder/master?urlpath=lab)

## Credits

This tool is forked from <https://github.com/yuvipanda/jupyter-desktop-server>.
It runs a Docker container at Binder, which includes websockify, a tightvnc server, and novnc via a jupyter notebook.



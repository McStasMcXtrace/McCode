#!/usr/bin/env python

import warnings
warnings.simplefilter('ignore')
import mcvine
warnings.simplefilter('default')

from mcvine.applications.InstrumentBuilder import build
components = ['source', 'sample', 'detector']
App = build(components)

def main():
    app = App('ssd')
    app.run()
    return

if __name__ == '__main__': main()

# $Id$

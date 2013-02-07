#!/usr/bin/python

import argparse

from subprocess import Popen, PIPE
from sys import stdin, stderr

from x3d import X3DWorld
from rewrite import parse_trace


def trace(instr, args, inspect=None):
    # run mcrun on instrument; capture stdout
    pid = Popen(['mcrun', '--trace', instr] + args, stdout=PIPE)

    # parse the trace from stdout
    world = parse_trace(X3DWorld(), pid.stdout, inspectComp=inspect)

    # wait for mcrun to finish
    pid.wait()

    # return traced 3d world
    return world



def main():
    parser = argparse.ArgumentParser()
    # Positional
    parser.add_argument('instrument', type=str, metavar='infile.instr',
                        help='the McStas instrument file')
    # Named
    parser.add_argument('--inspect', type=str, metavar='COMP',
                        help='only draw the neutrons that reach component COMP')
    parser.add_argument('--outfile', metavar='FILE', type=str, default='tmpfile.x3d',
                        help='write X3D code to FILE [default: tmpfile.x3d]')
    # Collect the rest and pass them to mcrun
    parser.add_argument('params', nargs=argparse.REMAINDER, metavar='...',
                        help='arguments to pass to mcrun (settings and parameters)')

    args = parser.parse_args()


    world = trace(args.instrument, args.params, inspect=args.inspect)
    file(args.outfile, 'w').write(world.dumps())



if __name__ == '__main__':
    main()

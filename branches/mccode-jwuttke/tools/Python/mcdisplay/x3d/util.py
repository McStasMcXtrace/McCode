''' Helper module to support McStas trace output processing '''

import os, sys
from math import pi, cos, sin
from numpy import dot, array
import subprocess


def parse_multiline(line):
    ''' Parse a multiline with size as first elements and n points as rest '''
    elems = [float(x) for x in line.split(',')]
    count = int(elems.pop(0))
    points = []
    while count > 0:
        points.append(elems[0:3])
        elems = elems[3:]
        count -= 1
    return points


def rotate(point, (origin, rotm)):
    ''' Rotate and move v according to origin and rotation matrix '''
    return dot(point, rotm) + origin


POINTS_IN_CIRCLE = 10
def draw_circle(plane, pos, radius, comp, out):
    ''' Draw a circle in plane, at pos and with r radius, rotated by comp '''
    points = []
    first = None
    for i in xrange(0, POINTS_IN_CIRCLE):
        walk = 2 * pi * i / POINTS_IN_CIRCLE
        xyz = array(pos)
        xyz[plane[0]] = cos(walk) * radius
        xyz[plane[1]] = sin(walk) * radius
        # rotate
        xyz = rotate(xyz, comp)
        # make sure lines are contiguous
        if first is None:
            first = array(xyz)
        for i in xrange(i == 0 and 1 or 2):
            points.append(xyz)
    # tie the knot
    points.append(first)
    out(points)



def get_line(fp):
    ''' Read a line from file-like object '''
    line = fp.readline()
    if line.startswith('Set value'):
        print line

    if line != '':
        return line.strip()


def debug(obj):
    ''' Write a Python object to stderr '''
    sys.stderr.write(repr(obj) + '\n')



def startfile(filepath):
    # Use built-in startfile if present (currently Windows-only)
    if hasattr(os, 'startfile'):
        return os.startfile(filepath)

    # Use 'open' on MAC and 'xdg-open' on Linux/*BSD
    # See: http://bugs.python.org/issue3177
    if sys.platform == 'darwin':
        command = ['open']
    else:
        command = ['xdg-open']

    # Run command and capture errors and return code
    ret = 0
    err = None
    try:
        ret = subprocess.call(command + [filepath])
    except OSError, e:
        err = e

    # Print error if something went wrong (couldn't find command or failed to run)
    if ret != 0 or err is not None:
        sys.stderr.write(
            'Error: Could not open file "%s" using "%s"\n' % (filepath, command[0]))
        if err:
            sys.stderr.write('Reason: %s\n' % err)

    # Only return 0 when all went well
    if err: return -1
    else: return ret

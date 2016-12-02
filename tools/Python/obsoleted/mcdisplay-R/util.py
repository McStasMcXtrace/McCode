''' Helper module to support McStas trace output processing '''

from sys import stdin, stderr
from math import pi, cos, sin
from numpy import dot, array


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


POINTS_IN_CIRCLE = 128
def draw_circle(plane, pos, radius, comp, out):
    ''' Draw a circle in plane, at pos and with r radius, rotated by comp '''
    first = None
    for i in xrange(0, POINTS_IN_CIRCLE):
        walk = 2 * pi * i / POINTS_IN_CIRCLE
        xyz = array(pos)
        xyz[plane[0]] += cos(walk) * radius
        xyz[plane[1]] += sin(walk) * radius
        # rotate
        xyz = rotate(xyz, comp)
        # make sure lines are contiguous
        if first is None:
            first = array(xyz)
        for i in xrange(i == 0 and 1 or 2):
            out(xyz)
    # tie the know
    out(first)



def get_line():
    ''' Read a line from stdin '''
    return stdin.readline().strip()


def debug(obj):
    ''' Write a Python object to stderr '''
    stderr.write(repr(obj) + '\n')

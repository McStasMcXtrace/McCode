''' Helper module to support McStas trace output processing '''

from sys import stdin, stderr
from math import pi, cos, sin
from numpy import dot, array
import numpy as np
from scipy.linalg import norm



def parse_multiline(line):
    ''' Parse a multiline with size as first elements and n points as rest '''
    elems = [float(x) for x in line.split(',')]
    count = int(elems.pop(0))
    points = []
    while count > 0:
        points.append(elems[0:3])
        elems = elems[3:]
        count -= 1

    points.append(points[0])
    return points


def rotate(point, inps):
    ''' Rotate and move v according to origin and rotation matrix '''
    (origin, rotm)=inps
    return dot(point, rotm) + origin

def rotate_points(points, inps):
    ''' Rotate and move v according to origin and rotation matrix '''
    (origin, rotm)=inps
    count = 0
    rpoints=[]
    x=[]
    y=[]
    z=[]
    while count < len(points):
        p=points[count]
        rpoints.append(rotate(p, (origin, rotm)))
        p=rpoints[count]
        x.append(p[0])
        y.append(p[1])
        z.append(p[2])
        count +=1
    x.append(x[0]);
    y.append(y[0]);
    z.append(z[0]);
    return x,y,z


def draw_sphere(center, radius):
    u = np.linspace(0, 2 * np.pi)
    v = np.linspace(0, np.pi)
    x = center[0] + radius * np.outer(np.cos(u), np.sin(v))
    y = center[1] + radius * np.outer(np.sin(u), np.sin(v))
    z = center[2] + radius * np.outer(np.ones(np.size(u)), np.cos(v))
    return x, y, z

def draw_cylinder(center, radius, height, axis_vector):
    axis_vector_normalized = axis_vector / np.linalg.norm(axis_vector)

    # Calculate half of the height vector
    half_height_vector = (height / 2) * axis_vector_normalized

    # Calculate the start and endpoint
    p0 = center - half_height_vector
    p1 = center + half_height_vector

    v = p1 - p0
    #find magnitude of vector
    mag = norm(v)
    #unit vector in direction of axis
    v = v/mag

    not_v = np.array([1, 0, 0])
    if (v == not_v).all():
        not_v = np.array([0,1, 0])

    #make vector perpendicular to v
    n1 = np.cross(v, not_v)
    #normalize n1
    n1 /= norm(n1)
    #make unit vector perpendicular to v and n1
    n2 = np.cross(v, n1)
    #surface ranges over t from 0 to height and 0 to 2*pi
    t = np.linspace(0, mag)
    theta = np.linspace(0, 2 * np.pi)
    #use meshgrid to make 2d arrays
    t, theta = np.meshgrid(t, theta)
    #generate coordinates for surface
    X, Y, Z = [p0[i] + v[i] * t + radius * np.sin(theta) * n1[i] + radius * np.cos(theta) * n2[i] for i in [0, 1, 2]]

    return X, Y, Z

POINTS_IN_CIRCLE = 128
def draw_circle(plane, pos, radius, comp):
    ''' Draw a circle in plane, at pos and with r radius, rotated by comp '''
    first = None
    x=[]
    y=[]
    z=[]
    for i in range(0, POINTS_IN_CIRCLE):
        walk = 2 * pi * i / POINTS_IN_CIRCLE
        xyz = array(pos)
        xyz[plane[0]] += cos(walk) * radius
        xyz[plane[1]] += sin(walk) * radius
        # rotate
        xyz = rotate(xyz, comp)
        x.append(xyz[0])
        y.append(xyz[1])
        z.append(xyz[2])
    x.append(x[0]);
    y.append(y[0]);
    z.append(z[0]);
    return x,y,z

def get_line():
    ''' Read a line from stdin '''
    return stdin.readline().strip()


def debug(obj):
    ''' Write a Python object to stderr '''
    stderr.write(repr(obj) + '\n')

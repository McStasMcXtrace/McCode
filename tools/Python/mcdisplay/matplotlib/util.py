''' Helper module to support McStas trace output processing '''

from sys import stdin, stderr
from math import pi, cos, sin
from numpy import dot, array
import numpy as np
from scipy.spatial.transform import Rotation as R

#level of detail in linspace
num_samples = 100


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
    x.append(x[0])
    y.append(y[0])
    z.append(z[0])
    return x, y, z

'''BEGIN NEW CODE 3D-visualization. REMOVE OLD CODE AND THIS COMMENT AFTER CONVERTING COMPS'''


def rotate_xyz(x, y, z, comp):
    for i in range(len(x)):
        for j in range(len(x)):
            point = np.array([x[i][j], y[i][j], z[i][j]])
            rotated_point = rotate(point, comp)
            x[i][j] = rotated_point[0]
            y[i][j] = rotated_point[1]
            z[i][j] = rotated_point[2]
    return x, y, z


def draw_sphere(center, radius):
    u = np.linspace(0, 2 * np.pi, num_samples)
    v = np.linspace(0, np.pi, num_samples)
    x = center[0] + radius * np.outer(np.cos(u), np.sin(v))
    y = center[1] + radius * np.outer(np.sin(u), np.sin(v))
    z = center[2] + radius * np.outer(np.ones(np.size(u)), np.cos(v))
    return x, y, z


def draw_cylinder(center, radius, height, axis_vector):
    axis_vector_normalized = axis_vector / np.linalg.norm(axis_vector)

    # Calculate half of the height vector
    half_height_vector = (height / 2) * axis_vector_normalized

    # Calculate the startpoint
    p0 = center - half_height_vector

    t = np.linspace(0, height, num_samples)
    theta = np.linspace(0, 2 * np.pi, num_samples)
    t, theta = np.meshgrid(t, theta)

    n1, n2 = calc_perp_vectors(axis_vector)

    x = p0[0] + axis_vector_normalized[0] * t + radius * np.sin(theta) * n1[0] + radius * np.cos(theta) * n2[0]
    y = p0[1] + axis_vector_normalized[1] * t + radius * np.sin(theta) * n1[1] + radius * np.cos(theta) * n2[1]
    z = p0[2] + axis_vector_normalized[2] * t + radius * np.sin(theta) * n1[2] + radius * np.cos(theta) * n2[2]

    return x, y, z


def draw_disc(center, radius, axis_vector):
    # Polar coordinates in the disc's plane
    theta = np.linspace(0, 2*np.pi, num_samples)
    r = np.linspace(0, radius, num_samples)
    theta, r = np.meshgrid(theta, r)

    # Calculate coordinates in the disc's plane
    x_plane = r * np.cos(theta)
    y_plane = r * np.sin(theta)

    (x, y, z) = center_and_align_with_axis_vector(center, x_plane, y_plane, axis_vector)

    return x, y, z


def draw_annulus(center, outer_radius, inner_radius, axis_vector):
    # Polar coordinates in the disc's plane
    theta = np.linspace(0, 2*np.pi, num_samples)
    r = np.linspace(0, inner_radius, num_samples)
    theta, r = np.meshgrid(theta, r)

    # Calculate coordinates in the disc's plane
    x_plane = (outer_radius-r) * np.cos(theta)
    y_plane = (outer_radius-r) * np.sin(theta)

    (x, y, z) = center_and_align_with_axis_vector(center, x_plane, y_plane, axis_vector)

    return x, y, z


def draw_cone(center, radius, height, axis_vector):
    # Define the grid in polar coordinates
    theta = np.linspace(0, 2 * np.pi, num_samples)
    z = np.linspace(-height/2, height/2, num_samples)
    theta, z = np.meshgrid(theta, z)

    # Convert polar to Cartesian coordinates (original, along z-axis)
    x_plane = (radius * (z - height/2) / height) * np.cos(theta)
    y_plane = (radius * (z - height/2) / height) * np.sin(theta)

    # Compute the two perpendicular vectors
    n1, n2 = calc_perp_vectors(axis_vector)

    # Transform these coordinates to align with the given axis_vector
    x = center[0] + x_plane * n1[0] + y_plane * n2[0] + z * axis_vector[0]
    y = center[1] + x_plane * n1[1] + y_plane * n2[1] + z * axis_vector[1]
    z = center[2] + x_plane * n1[2] + y_plane * n2[2] + z * axis_vector[2]

    return x, y, z


def draw_box(center, a, b, c):
    #spherical coordinates cube
    phi = np.arange(1, 10, 2)*np.pi/4
    phi, theta = np.meshgrid(phi, phi)
    x = center[0] + (np.cos(phi)*np.sin(theta))*a
    y = center[1] + (np.sin(phi)*np.sin(theta))*b
    z = center[2] + (np.cos(theta)/np.sqrt(2))*c

    return x, y, z


def draw_hollow_box(center, a, b, c):
    phi = np.arange(1, 10, 2)*np.pi/4
    phi, theta = np.meshgrid(phi, phi)
    x = center[0] + np.cos(phi)*a
    y = center[1] + np.sin(phi)*b
    z = center[2] + (np.cos(theta)/np.sqrt(2))*c

    return x, y, z

def draw_rectangular_lid(center, outer_a, outer_b, inner_a, inner_b):
    phi = np.arange(1, 10, 2)*np.pi/4
    phi, theta = np.meshgrid(phi, phi)
    x = center[0] + (outer_a-phi) * np.cos(phi)*inner_a
    y = center[1] + (outer_b-phi) * np.sin(phi)*inner_b
    z = center[2] + x + y

    return x, y, z

def draw_new_circle(center, radius, axis_vector):
    theta = np.linspace(0, 2 * np.pi, num_samples)
    theta = np.meshgrid(theta, theta)

    x_plane = center[0] + radius * np.cos(theta)
    y_plane = center[1] + radius * np.sin(theta)

    (x, y, z) = center_and_align_with_axis_vector(center, x_plane, y_plane, axis_vector)

    return x, y, z

def center_and_align_with_axis_vector(center, x_plane, y_plane, axis_vector):
    # Calculate perpendicular vectors
    n1, n2 = calc_perp_vectors(axis_vector)
    # Transform the coordinates
    x = center[0] + x_plane * n1[0] + y_plane * n2[0]
    y = center[1] + x_plane * n1[1] + y_plane * n2[1]
    z = center[2] + x_plane * n1[2] + y_plane * n2[2]
    return x, y, z

def calc_perp_vectors(axis_vector):
    # Normalize the axis vector
    axis_vector_normalized = axis_vector / np.linalg.norm(axis_vector)
    # Create an arbitrary vector perpendicular to the axis vector
    if (axis_vector_normalized == np.array([1, 0, 0])).all():
        not_v = np.array([0, 1, 0])
    else:
        not_v = np.array([1, 0, 0])
    # Compute two orthogonal vectors in the plane perpendicular to the axis vector
    n1 = np.cross(axis_vector_normalized, not_v)
    n1 /= np.linalg.norm(n1)  # Normalize n1
    n2 = np.cross(axis_vector_normalized, n1)
    return n1, n2

'''END NEW CODE 3D-visualization. REMOVE OLD CODE AND THIS COMMENT AFTER CONVERTING COMPS'''

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

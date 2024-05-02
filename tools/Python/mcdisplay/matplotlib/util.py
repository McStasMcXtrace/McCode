''' Helper module to support McStas trace output processing '''

from sys import stdin, stderr
from math import pi, cos, sin

from matplotlib.patches import Circle
from mpl_toolkits.mplot3d import art3d
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
from numpy import dot, array
import numpy as np
from scipy.linalg import norm
from scipy.spatial.transform import Rotation as R
from scipy.spatial import ConvexHull

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
    x.append(x[0]);
    y.append(y[0]);
    z.append(z[0]);
    return x,y,z

'''BEGIN NEW CODE 3D-visualization. REMOVE OLD CODE AND THIS COMMENT AFTER CONVERTING COMPS'''

''' Parse a polygon with size as first elements and n points as rest '''
def parse_polygon(line):
    elems = [float(x) for x in line.split(',')]
    count = int(elems.pop(0))
    points = []
    while count > 0:
        points.append(elems[0:3])
        elems = elems[3:]
        count -= 1

    points.append(points[0])
    return points

def rotate_xyz(x, y, z, comp):
    for i in range(len(x)):
        for j in range(len(x)):
            point = np.array([x[i][j], y[i][j], z[i][j]])
            rotated_point = rotate(point, comp)
            x[i][j] = rotated_point[0]
            y[i][j] = rotated_point[1]
            z[i][j] = rotated_point[2]
    return (x, y, z)

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
    t = np.linspace(0, mag, num_samples)
    theta = np.linspace(0, 2 * np.pi, num_samples)
    #use meshgrid to make 2d arrays
    t, theta = np.meshgrid(t, theta)
    #generate coordinates for surface
    x, y, z = [p0[i] + v[i] * t + radius * np.sin(theta) * n1[i] + radius * np.cos(theta) * n2[i] for i in [0, 1, 2]]

    return x, y, z

def draw_disc(center, radius, axis_vector):
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

    # Polar coordinates in the disc's plane
    theta = np.linspace(0, 2*np.pi, num_samples)
    r = np.linspace(0, radius, num_samples)
    theta, r = np.meshgrid(theta, r)

    # Calculate coordinates in the disc's plane
    x_plane = r * np.cos(theta)
    y_plane = r * np.sin(theta)

    # Transform these coordinates to align with the given axis_vector
    x = center[0] + x_plane * n1[0] + y_plane * n2[0]
    y = center[1] + x_plane * n1[1] + y_plane * n2[1]
    z = center[2] + x_plane * n1[2] + y_plane * n2[2]

    return x, y, z

def draw_cone(center, radius, height, axis_vector):
    # Normalize the axis vector
    axis_vector_normalized = axis_vector / np.linalg.norm(axis_vector)

    # Axis of rotation (cross product of z-axis and v) and angle of rotation
    z_axis = np.array([0, 0, 1])
    axis = np.cross(z_axis, axis_vector_normalized)
    angle = np.arccos(np.dot(z_axis, axis_vector_normalized))
    # Generate rotation matrix using scipy's Rotation
    rotation = R.from_rotvec(axis * angle)
    rot_matrix = rotation.as_matrix()

    # Define the grid in polar coordinates
    theta = np.linspace(0, 2 * np.pi, num_samples)
    z = np.linspace(-height/2, height/2, num_samples)  # Adjusted to center around 0 along z
    theta, z = np.meshgrid(theta, z)

    # Convert polar to Cartesian coordinates (original, along z-axis)
    x = (radius * (z + height/2) / height) * np.cos(theta)  # Adjusted radius calculation
    y = (radius * (z + height/2) / height) * np.sin(theta)  # Adjusted radius calculation

    # Rotate the coordinates
    xyz = np.stack([x.flatten(), y.flatten(), z.flatten()])
    xyz_rotated = rot_matrix @ xyz

    # Reshape back to original dimensions
    x_rotated, y_rotated, z_rotated = xyz_rotated.reshape(3, *x.shape)

    # Translate to center the midpoint of the cone at the given center
    x = x_rotated + center[0]
    y = y_rotated + center[1]
    z = z_rotated + center[2]

    return x, y, z

def draw_box(center, a, b, c):
    #spherical coordinates cube
    phi = np.arange(1,10,2)*np.pi/4
    Phi, Theta = np.meshgrid(phi, phi)
    x = center[0] + (np.cos(Phi)*np.sin(Theta))*a
    y = center[1] + (np.sin(Phi)*np.sin(Theta))*b
    z = center[2] + (np.cos(Theta)/np.sqrt(2))*c

    return x, y, z

def draw_polygon(points, ax):

    plt.show()
    x = center[0]
    y = center[1]
    z = center[2]

    return x, y, z

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
    line = stdin.readline().strip()
    print(line)
    return line


def debug(obj):
    ''' Write a Python object to stderr '''
    stderr.write(repr(obj) + '\n')

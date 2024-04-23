#!/usr/bin/env python

''' Small script to rewrite McStas trace output to python matplotlib for plotting '''

import sys
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

from util import parse_multiline, rotate, rotate_points, draw_circle, get_line, debug, draw_sphere, draw_cylinder

UC_COMP = 'COMPONENT:'

MC_COMP = 'MCDISPLAY: component'
MC_COMP_SHORT = 'COMP: '

MC_LINE = 'MCDISPLAY: multiline'
MC_CIRCLE = 'MCDISPLAY: circle'
MC_SPHERE = 'MCDISPLAY: sphere'
MC_CYLINDER = 'MCDISPLAY: cylinder'
MC_BOX = 'MCDISPLAY: box'

MC_ENTER = 'ENTER:'
MC_LEAVE = 'LEAVE:'
MC_STATE = 'STATE:'
MC_SCATTER = 'SCATTER:'
MC_ABSORB = 'ABSORB:'
MC_MAGNIFY = 'MCDISPLAY: magnify'
MC_START = 'MCDISPLAY: start'
MC_END = 'MCDISPLAY: end'
MC_STOP = 'INSTRUMENT END:'


def parse_trace():
    ''' Parse McStas trace output from stdin and write results
        to file objects csv_comps and csv_lines '''

    mpl.rcParams['legend.fontsize'] = 10

    ax = plt.figure(figsize=plt.figaspect(0.5)*1.5).add_subplot(projection='3d')
    ax.set(xlabel='z', ylabel='x', zlabel='y')
    try:
        ax.set_aspect('equal')
    except:
        print("manual aspect not supported")

    color = 0

    # map from component name to (position, rotation matrix)
    comps = {}

    # active (position, rotation matrix)
    comp = (np.array([0, 0, 0]),
            np.array([1, 0, 0,
                      0, 1, 0,
                      0, 0, 1]).reshape(3,3))

    # previous neutron position
    prev = None
    skip = False
    # we are drawing a neutron
    active = False
    xstate=[]
    ystate=[]
    zstate=[]
    
    while True:
        # read line
        line = get_line()
        if not line:
            break

        # register components
        if line.startswith(UC_COMP):
            # grab info line
            info = get_line()
            assert info[:4] == 'POS:'
            nums = [x.strip() for x in info[4:].split(',')]
            # extract fields
            name = line[len(UC_COMP):].strip(' "\n')
            pos = np.array([float(x) for x in nums[:3]])
            # read flat 3x3 rotation matrix
            rot = np.array([float(x) for x in nums[3:3+9]]).reshape(3, 3)
            comps[name] = (pos, rot)

        # switch perspective
        elif line.startswith(MC_COMP):
            color += 1
            comp = comps[line[len(MC_COMP) + 1:]]

        elif line.startswith(MC_COMP_SHORT):
            name = line[len(MC_COMP_SHORT) + 1:].strip('"')
            comp = comps[name]
            skip = True

        # process multiline
        elif line.startswith(MC_LINE):
            points = parse_multiline(line[len(MC_LINE):].strip('()'))
            start = points.pop(0)
            (x, y, z) = rotate_points(points, comp)
            ax.plot(z, x, y)

        # process circle
        elif line.startswith(MC_CIRCLE):
            xyz = 'xyz'
            items = line[len(MC_CIRCLE):].strip('()').split(',')
            # plane
            pla = [xyz.find(a) for a in items[0].strip("''")]
            # center and radius
            pos = [float(x) for x in items[1:4]]
            rad = float(items[4])
            (x,y,z) = draw_circle(pla, pos, rad, comp)
            ax.plot(z, x, y)

        # process sphere
        elif line.startswith(MC_SPHERE):
            process_sphere(ax, line, comp)

        # process box
        #elif line.startswith(MC_BOX):
        #process_box(ax, line)

        # process cylinder
        elif line.startswith(MC_CYLINDER):
            process_cylinder(ax, line, comp)

        # activate neutron when it enters
        elif line.startswith(MC_ENTER):
            prev = None
            skip = True
            active = True
            color = 0
            xstate=[]
            ystate=[]
            zstate=[]

        # deactivate neutron when it leaves
        elif line.startswith(MC_LEAVE):
            ax.plot(zstate, xstate, ystate)
            active = False
            prev = None
            
        elif line.startswith(MC_ABSORB):
            pass

        # register state and scatter
        elif line.startswith(MC_STATE) or line.startswith(MC_SCATTER):
            
            if not active:
                continue

            if skip:
                skip = False
                continue

            register_state_and_scatter(comp, line, prev, xstate, ystate, zstate)

        # kick out legacy "junk"
        elif line.startswith(MC_MAGNIFY) or line.startswith(MC_START) or line.startswith(MC_END) or line.startswith(MC_STOP):
            continue
        else:
            print(line)

    set_axis_limits(ax)

    plt.show()


def process_sphere(ax, line, comp):
    items = line[len(MC_SPHERE):].strip('()').split(',')
    # center and radius
    center = [float(x) for x in items[1:4]]
    radius = float(items[3])
    (x, y, z) = draw_sphere(center, radius)
    (x, y, z) = rotate_xyz(x, y, z, comp)
    ax.plot_surface(z,x,y)


def rotate_xyz(x, y, z, comp):
    for i in range(len(x)):
        for j in range(len(x)):
            point = np.array([x[i][j], y[i][j], z[i][j]])
            rotated_point = rotate(point, comp)
            x[i][j] = rotated_point[0]
            y[i][j] = rotated_point[1]
            z[i][j] = rotated_point[2]
    return (x, y, z)



def process_cylinder(ax, line, comp):
    items = line[len(MC_CYLINDER):].strip('()').split(',')
    center = [float(x) for x in items[1:4]]
    rad = float(items[3])
    height = float(items[4])
    axis_vector=[float(x) for x in items[6:9]]
    (x, y, z) = draw_cylinder(center, rad, height, axis_vector)
    (x, y, z) = rotate_xyz(x, y, z, comp)
    ax.plot_surface(z,x,y)

'''
def process_box(ax, line):
    items = line[len(MC_BOX):].strip('()').split(',')
    center = [float(x) for x in items[1:4]]

    ax.plot_surface(x,y,z)
'''

def register_state_and_scatter(comp, line, prev, xstate, ystate, zstate):
    xyz = [float(x) for x in line[line.find(':') + 1:].split(',')[:3]]
    xyz = rotate(xyz, comp)
    if prev is not None:
        xstate.append(xyz[0])
        ystate.append(xyz[1])
        zstate.append(xyz[2])
    prev = xyz
    xstate.append(prev[0])
    ystate.append(prev[1])
    zstate.append(prev[2])


def set_axis_limits(ax):
    # A little bit of logic for controlling the aspect ratios/view
    (xmin, xmax) = ax.get_xlim()
    (ymin, ymax) = ax.get_ylim()
    (zmin, zmax) = ax.get_zlim()
    dx = xmax - xmin
    dy = ymax - ymin
    dz = zmax - zmin
    dmax = max(dx, dy, dz)
    # Check ranges and define axis box of max length cubed
    if dmax > dx:
        mean = (xmax + xmin) / 2
        xmin = mean - dmax / 2
        xmax = mean + dmax / 2
    if dmax > dy:
        mean = (ymax + ymin) / 2
        ymin = mean - dmax / 2
        ymax = mean + dmax / 2
    if dmax > dz:
        mean = (zmax + zmin) / 2
        zmin = mean - dmax / 2
        zmax = mean + dmax / 2
    # Set new axis limits
    ax.set_xlim3d(xmin, xmax)
    ax.set_ylim3d(ymin, ymax)
    ax.set_zlim3d(zmin, zmax)


if __name__ == '__main__':
    parse_trace()

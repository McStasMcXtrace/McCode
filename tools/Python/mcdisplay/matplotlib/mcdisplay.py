#!/usr/bin/env python

''' Small script to rewrite McStas trace output to python matplotlib for plotting '''

import sys
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

from util import parse_multiline, rotate, rotate_points, draw_circle, get_line, debug


UC_COMP = 'COMPONENT:'

MC_COMP       = 'MCDISPLAY: component'
MC_COMP_SHORT = 'COMP: '

MC_LINE = 'MCDISPLAY: multiline'
MC_CIRCLE = 'MCDISPLAY: circle'

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

    fig = plt.figure(figsize=plt.figaspect(0.5)*1.5)
    ax = fig.gca()
#    ax.xlabel("z")
#    ax.ylabel("x")
#    ax.zlabel("y")
    try:
        ax.set_aspect('equal')
    except:
        print("manual aspect not supported")
    #    ax.autoscale_view(scalex=False, scaley=False, scalez=False)
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
            
            xyz = [float(x) for x in line[line.find(':')+1:].split(',')[:3]]
            xyz = rotate(xyz, comp)
            if prev is not None:
                xstate.append(xyz[0])
                ystate.append(xyz[1])
                zstate.append(xyz[2])
            prev = xyz
            xstate.append(prev[0])
            ystate.append(prev[1])
            zstate.append(prev[2])

        # kick out legacy "junk"
        elif line.startswith(MC_MAGNIFY) or line.startswith(MC_START) or line.startswith(MC_END) or line.startswith(MC_STOP):
            continue
        else:
            print(line)


    # A little bit of logic for controlling the aspect ratios/view
    (xmin, xmax)=ax.get_xlim()
    (ymin, ymax)=ax.get_ylim()
    (zmin, zmax)=ax.get_zlim()
    dx = xmax - xmin
    dy = ymax - ymin
    dz = zmax - zmin
    dmax=max(dx,dy,dz)

    # Check ranges and define axis box of max length cubed
    if dmax > dx:
        mean=(xmax+xmin)/2
        xmin=mean-dmax/2
        xmax=mean+dmax/2
    if dmax > dy:
        mean=(ymax+ymin)/2
        ymin=mean-dmax/2
        ymax=mean+dmax/2
    if dmax > dz:
        mean=(zmax+zmin)/2
        zmin=mean-dmax/2
        zmax=mean+dmax/2
        
    # Set new axis limits
    ax.set_xlim3d(xmin,xmax)
    ax.set_ylim3d(ymin,ymax)
    ax.set_zlim3d(zmin,zmax)
 
    plt.show()

if __name__ == '__main__':
    parse_trace()

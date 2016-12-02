#!/usr/bin/env python

''' Small script to rewrite McStas trace output to CSV data for plotting '''

import argparse

import sys
import numpy as np

import x3d
from util import parse_multiline, rotate, get_line, debug, draw_circle


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


colors = ["1.0, 0.0, 0.0","0.0, 1.0, 0.0","0.0, 0.0, 1.0",
          "1.0, 1.0, 0.0","1.0, 0.0, 1.0","0.0, 1.0, 1.0",
          "1.0, 1.0, 1.0","0.5, 1.0, 1.0","1.0, 0.5, 1.0",
          "1.0, 1.0, 0.5","0.5, 0.0, 1.0","0.0, 0.5, 1.0",
          "0.0, 1.0, 0.5","0.5, 1.0, 0.0","1.0, 0.5, 0.0",
          "1.0, 0.0, 0.5","0.5, 0.0, 0.0","0.0, 0.5, 0.0",
          "0.0, 0.0, 0.5","0.5, 0.5, 1.0","0.5, 1.0, 0.5",
          "1.0, 0.5, 0.5","0.5, 0.0, 0.5","0.0, 0.5, 0.5",
          "0.5, 0.5, 0.0","0.5, 0.5, 0.5"]

def getColor(n):
    return colors[n % len(colors)]


def parse_trace(world, fp=sys.stdin, inspectComp=None):
    ''' Prase McStas trace output from stdin and write result to output '''

    color = 0

    # def out_point((p_x, p_y, p_z)):
    #     ''' Write a line to csv_lines '''
    #     csv_lines.write('%s, %s, %s, %s\n' % (p_x, p_y, p_z, color))

    # print headers
    # csv_comps.write('name, x, y, z\n')
    # csv_lines.write('x, y, z, c\n')

    # map from component name to (position, rotation matrix)
    comps = {}

    # active (position, rotation matrix)
    comp = (np.array([0, 0, 0]),
            np.array([1, 0, 0,
                      0, 1, 0,
                      0, 0, 1]).reshape(3,3))
    compName = ""


    # we are following a neutron
    active = False
    # we need to draw the neutron (it passed the "check-point"/inspect component)
    inspect = False
    # list of observed neutron positions
    neutron = []
    # skip next neutron position
    skip = False
    # total count of drawed neutrons
    neutrons_drawed = 0


    while True:
        # read line
        line = get_line(fp)
        if line is None:
            break

        # register components
        if line.startswith(UC_COMP):
            # grab info line
            info = get_line(fp)
            assert info[:4] == 'POS:'
            nums = [x.strip() for x in info[4:].split(',')]
            # extract fields
            name = line[len(UC_COMP):].strip(' "\n')
            pos = np.array([float(x) for x in nums[:3]])
            # read flat 3x3 rotation matrix
            rot = np.array([float(x) for x in nums[3:3+9]]).reshape(3, 3)
            comps[name] = (pos, rot)
            # csv_comps.write('%s, %s, %s, %s\n' % ((name,) + tuple(pos)))

        # switch perspective
        elif line.startswith(MC_COMP):
            color += 1
            name = line[len(MC_COMP) + 1:].strip()
            compName = name
            comp = comps[name]

        elif line.startswith(MC_COMP_SHORT):
            name = line[len(MC_COMP_SHORT) + 1:].strip('"')
            compName = name
            comp = comps[name]
            skip = True

        # process multiline
        elif line.startswith(MC_LINE):
            points = parse_multiline(line[len(MC_LINE):].strip('()'))
            world.drawLine((rotate(p, comp) for p in points), color=getColor(color))

        # process circle
        elif line.startswith(MC_CIRCLE):
            xyz = 'xyz'
            items = line[len(MC_CIRCLE):].strip('()').split(',')
            # plane
            pla = [xyz.find(a) for a in items[0].strip("''")]
            # center and radius
            pos = [float(x) for x in items[1:4]]
            rad = float(items[4])
            points = draw_circle(pla, pos, rad, comp)
            world.drawLine(points, color=getColor(color))

        # activate neutron when it enters
        elif line.startswith(MC_ENTER):
            neutron = []
            skip = True
            active = True
            inspect = False
            color += 1

        # deactivate neutron when it leaves
        elif line.startswith(MC_LEAVE) or line.startswith(MC_ABSORB):
            active = False
            if inspectComp is None or inspect:
                world.drawLine(neutron, color="1 0 0")
                neutrons_drawed += 1

        # register state and scatter
        elif line.startswith(MC_STATE) or line.startswith(MC_SCATTER):

            if not active:
                continue

            if skip:
                skip = False
                continue

            if inspectComp and inspectComp == compName:
                # We will draw this neutron!
                inspect = True

            # keep track of points the neutron passes through
            xyz = [float(x) for x in line[line.find(':')+1:].split(',')[:3]]
            xyz = rotate(xyz, comp)
            neutron.append(xyz)

    print('Neutrons drawed:', neutrons_drawed, (inspectComp and '(reaching %s)' % inspectComp or '(all)'))
    return world

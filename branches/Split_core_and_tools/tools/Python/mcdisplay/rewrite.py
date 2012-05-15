#!/usr/bin/env python

''' Small script to rewrite McStas trace output to CSV data for plotting '''

import sys
import numpy as np

from util import parse_multiline, rotate, draw_circle, get_line, debug


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


def parse_trace(csv_comps, csv_lines):
    ''' Prase McStas trace output from stdin and write results
        to file objects csv_comps and csv_lines '''

    color = 0
    def out_point((p_x, p_y, p_z)):
        ''' Write a line to csv_lines '''
        csv_lines.write('%s, %s, %s, %s\n' % (p_x, p_y, p_z, color))

    # print headers
    csv_comps.write('name, x, y, z\n')
    csv_lines.write('x, y, z, c\n')

    # map from component name to (position, rotation matrix)
    comps = {}

    # active (position, rotation matrix)
    comp = (np.array([0, 0, 0]),
            np.array([1, 0, 0,
                      0, 1, 0,
                      0, 0, 1]).reshape(3,3))

    # previous neutron position
    prev = None
    count = 0
    # we are drawing a neutron
    active = False

    # read the first line
    line = get_line()

    while line:
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
            csv_comps.write('%s, %s, %s, %s\n' % ((name,) + tuple(pos)))

        # switch perspective
        elif line.startswith(MC_COMP):
            color += 1
            comp = comps[line[len(MC_COMP) + 1:]]

        elif line.startswith(MC_COMP_SHORT):
            comp = comps[line[len(MC_COMP_SHORT) + 1:].strip('"')]

        # process multiline
        elif line.startswith(MC_LINE):
            points = parse_multiline(line[len(MC_LINE):].strip('()'))
            start = points.pop(0)
            while points:
                end = points.pop(0)
                for point in (start, end):
                    out_point(rotate(point, comp))
                start = end

        # process circle
        elif line.startswith(MC_CIRCLE):
            xyz = 'xyz'
            items = line[len(MC_CIRCLE):].strip('()').split(',')
            # plane
            pla = [xyz.find(a) for a in items[0].strip("''")]
            # center and radius
            pos = [float(x) for x in items[1:4]]
            rad = float(items[4])
            draw_circle(pla, pos, rad, comp, out_point)

        # activate neutron when it enters
        elif line.startswith(MC_ENTER):
            count = 0
            prev = None
            active = True
            color += 1

        # deactivate neutron when it leaves
        elif line.startswith(MC_LEAVE):
            active = False

        elif line.startswith(MC_ABSORB):
            prev = None

        # register state and scatter
        elif line.startswith(MC_STATE) or line.startswith(MC_SCATTER):
            if active:
                xyz = [float(x) for x in line[line.find(':')+1:].split(',')[:3]]
                xyz = rotate(xyz, comp)
                if prev is not None and count > 1:
                    pass
                elif count > 0:
                    prev = xyz
                count += 1

        line = get_line()


if __name__ == '__main__':
    parse_trace(csv_comps=file(sys.argv[1], 'w'),
                csv_lines=file(sys.argv[2], 'w'))

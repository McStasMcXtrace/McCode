#!/usr/bin/env vtkpython

''' Script to rewrite McStas trace output to VTK for plotting '''

# initially from python/mcdisplay/matplotlib
# ported to VTK by Gael Goret, Eric Pellegrini and Bachir Aoun, nMoldyn project, ILL/CS 2014
# 

import numpy as np
import vtk
import sys

from util import parse_multiline, rotate, rotate_points, draw_circle


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


def parse_trace(fname):
    ''' Parse McStas trace output from stdin and write results
        to file objects csv_comps and csv_lines '''

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
    
    circlePoints = vtk.vtkPoints()
    circleLines = vtk.vtkCellArray()
    circle_pid = 0
    
    multiPoints = vtk.vtkPoints()
    multiLines = vtk.vtkCellArray()
    multi_pid = 0
    
    neutronPoints = vtk.vtkPoints()
    neutronLines = vtk.vtkCellArray()
    neutron_pid = 0
    
    f = open(fname, 'r')
    lines = f.readlines()
    
    for i, line in enumerate(lines):
        if not line:
            break
        line = line.strip()
        # register components
        if line.startswith(UC_COMP):
            # grab info line
            info = lines[i+1]
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
            points.pop(0)
            coords = rotate_points(points, comp)
            beg = multi_pid
            for p in coords:
                multiPoints.InsertNextPoint(p)
                multi_pid += 1
            end = multi_pid
            for idx in range(beg, end-1):
                vline = vtk.vtkLine()
                vline.GetPointIds().SetId(0,idx)
                vline.GetPointIds().SetId(1,idx +1)
                multiLines.InsertNextCell(vline)
                
        # process circle
        elif line.startswith(MC_CIRCLE):
            xyz = 'xyz'
            items = line[len(MC_CIRCLE):].strip('()').split(',')
            # plane
            pla = [xyz.find(a) for a in items[0].strip("''")]
            # center and radius
            pos = [float(x) for x in items[1:4]]
            rad = float(items[4])
            coords = draw_circle(pla, pos, rad, comp)
            beg = circle_pid
            for p in coords:
                circlePoints.InsertNextPoint(p)
                circle_pid += 1
            end = circle_pid
            for idx in range(beg, end-1):
                vline = vtk.vtkLine()
                vline.GetPointIds().SetId(0,idx)
                vline.GetPointIds().SetId(1,idx +1)
                circleLines.InsertNextCell(vline)
        
                
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
            
            coords = np.column_stack([xstate, ystate, zstate])
            beg = neutron_pid
            for p in coords:
                neutronPoints.InsertNextPoint(p)
                neutron_pid += 1
            end = neutron_pid
            for idx in range(beg, end-1):
                vline = vtk.vtkLine()
                vline.GetPointIds().SetId(0,idx)
                vline.GetPointIds().SetId(1,idx +1)
                neutronLines.InsertNextCell(vline)
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

    f.close()

    circlePolydata =vtk.vtkPolyData()
    circlePolydata.SetPoints(circlePoints)
    circlePolydata.SetLines(circleLines)

    circle_mapper = vtk.vtkPolyDataMapper()
    try:
      circle_mapper.SetInputData(circlePolydata) # VTK Python >= 6
    except:
      circle_mapper.SetInput(circlePolydata)     # VTK Python >= 5.8
    circle_actor = vtk.vtkActor()
    circle_actor.SetMapper(circle_mapper)
    circle_actor.GetProperty().SetAmbient(0.2)
    circle_actor.GetProperty().SetDiffuse(0.5)
    circle_actor.GetProperty().SetSpecular(0.3)
    circle_actor.GetProperty().SetColor(0,0.7,0.7)
    circle_actor.GetProperty().SetLineWidth(3)   
    
    
    multiPolydata =vtk.vtkPolyData()
    multiPolydata.SetPoints(multiPoints)
    multiPolydata.SetLines(multiLines)

    multi_mapper = vtk.vtkPolyDataMapper()
    try:
      multi_mapper.SetInputData(multiPolydata)
    except:
      multi_mapper.SetInput(multiPolydata)
    multi_actor = vtk.vtkActor()
    multi_actor.SetMapper(multi_mapper)
    multi_actor.GetProperty().SetAmbient(0.2)
    multi_actor.GetProperty().SetDiffuse(0.5)
    multi_actor.GetProperty().SetSpecular(0.3)
    multi_actor.GetProperty().SetColor(1,0,0.5)
    multi_actor.GetProperty().SetLineWidth(3)
    
    neutronPolydata =vtk.vtkPolyData()
    neutronPolydata.SetPoints(neutronPoints)
    neutronPolydata.SetLines(neutronLines)

    neutron_mapper = vtk.vtkPolyDataMapper()
    try:
      neutron_mapper.SetInputData(neutronPolydata)
    except:
      neutron_mapper.SetInput(neutronPolydata)
    neutron_actor = vtk.vtkActor()
    neutron_actor.SetMapper(neutron_mapper)
    neutron_actor.GetProperty().SetAmbient(0.2)
    neutron_actor.GetProperty().SetDiffuse(0.5)
    neutron_actor.GetProperty().SetSpecular(0.3)
    neutron_actor.GetProperty().SetColor(1,1,1)
    neutron_actor.GetProperty().SetLineWidth(2) 
    
    renderer = vtk.vtkRenderer()
    renderer.AddActor(circle_actor)
    renderer.AddActor(multi_actor)
    renderer.AddActor(neutron_actor)
    renderer.SetBackground(0, 0, 0)
    
    renwin = vtk.vtkRenderWindow()
    renwin.AddRenderer(renderer)
    
    iren = vtk.vtkRenderWindowInteractor()
    istyle = vtk.vtkInteractorStyleTrackballCamera() 
    iren.SetInteractorStyle(istyle)
    iren.SetRenderWindow(renwin)

    iren.Initialize()
    renwin.Render()
    iren.Start()    
 

    
if __name__ == '__main__':
    parse_trace(sys.argv[1])

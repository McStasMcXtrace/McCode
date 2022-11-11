#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
Converts MCDISPLAY geometry information, read from an instrument binary, to STEP format.
Uses the CadQuery package, https://cadquery.readthedocs.io, and its interface to
OpenCASCADE to handle some operations, most importantly, the saving to STEP file.
'''
__author__ = "Gregory Tucker"
__date__ = "2022-11-11"

import sys
from pathlib import Path

sys.path.append(str(Path(__file__).resolve().parent.parent.parent))
from mccodelib.instrgeom import Vector3d, DrawLine, DrawMultiline, DrawCircle


def vector_to_tuple(v: Vector3d):
    return v.x, v.y, v.z


def transform_shape(global_transformation_matrix, local_shape):
    """Perform the coordinate transformation for a shape
    
    >>> placed = transform_shape(global_transformation_matrix, local_shape)

    Including handling of various possible input shape types and transformation types
    """
    # Import types which the global transformation matrix could be:
    from cadquery.occ_impl.geom import Matrix
    from OCP.gp import gp_Trsf, gp_GTrsf  # plus more transformatation types?
    # Import types which the local shape could be:
    from cadquery.occ_impl.shapes import Compound, Shape
    from cadquery.cq import Workplane
    from OCP.TopoDS import TopoDS_Compound, TopoDS_Shape

    # Get a handle to the underlying OpenCascade shape object, depending on what the input type is
    global_shape = None
    if isinstance(local_shape, Workplane):
        global_shape = local_shape.toOCC()
    elif isinstance(local_shape, (Shape, Compound)):
        global_shape = local_shape.wrapped
    elif insinstance(local_shape, (TopoDS_Shape, TopoDS_Compound)):
        # We are already a suitable type for local to global transformation
        global_shape = local_shape 
    else:
        print(f"Unknown shape type {type(local_shape)}, attempting the 'wrapped' property, expect errors!")
        global_shape = local_shape.wrapped

    transformation = None
    if isinstance(global_transformation_matrix, Matrix):
        transformation = global_transformation_matrix.wrapped
    elif isinstance(global_transformation_matrix, (gp_Trsf, gp_GTrsf)):
        # Already a suitable transformation type
        transformation = global_transformation_matrix
    else:
        gtmt = type(global_transformation_matrix)
        print(f"Unknown transformation implementation type {gtmt}, attemping the 'wrapped' property, expect errors!")
        transformation = global_transformation_matrix.wrapped

    # Now decide which transformation algorithm can be used based on what the input transformation represents
    if isinstance(transformation, gp_Trsf):
        # This is a unitary transformation and can only reposition/reorient the shape:
        from OCP.BRepBuilderAPI import BRepBuilderAPI_Transform as Trans
        global_shape = Trans(global_shape, transformation).Shape()
    elif isinstance(transformation, gp_GTrsf):
        # This is a general transformation which can additionally rescale/stretch/skew the shape:
        from OCP.BRepBuilderAPI import BRepBuilderAPI_GTransform as GTrans
        global_shape = GTrans(global_shape, transformation).Shape()
    else:
        print(f"Unknown transformation type {type(matrix)}. No transformation applied!")

    # (Re)wrap the OpenCascade shape back to a cadquery.Shape
    shape = Shape(global_shape)
    return shape


def circle_to_brep(comp: DrawCircle):
    from cadquery import Workplane
    plane = comp.plane.upper()
    center = vector_to_tuple(comp.center)
    radius = comp.radius  # / meter
    w = Workplane(plane, origin=center).circle(radius)
    return w


def multiline_to_brep(comp: DrawMultiline):
    from cadquery import Workplane
    vs = [vector_to_tuple(x) for x in comp.points]

    bad = [vs[i] == vs[i+1] for i in range(len(vs)-1)]
    while any(bad):
        first = [idx for idx, b in enumerate(bad) if b][0]
        vs = vs[:first] + vs[first+1:]
        bad = [vs[i] == vs[i+1] for i in range(len(vs)-1)]
    
    w = None
    if len(vs) > 1:
        w = Workplane('XY').polyline(vs)
    return w


def drawcalls_to_shape(drawcalls):
    from cadquery import Assembly
    a = Assembly()
    for d in drawcalls:
        if isinstance(d, DrawCircle):
            brep = circle_to_brep(d)
        elif isinstance(d, (DrawLine, DrawMultiline)):
            brep = multiline_to_brep(d)
        else:
            print(f"Drawing of {type(d)} not supported; please extend {__file__}!")
            print('\n'.join([f'\t{x}' for x in dir(d) if x[0] != '_']))
            brep = None
        if brep is not None:
            a = a.add(brep)

    a = a.toCompound()
    return a


def component_to_brep(comp):
    from cadquery import Matrix
    orient = Matrix([[comp.m4[i+j*4] for i in range(4)] for j in range(3)])
    # A more-complex system would check if the draw calls of the component are, e.g.,
    #   - three perpendicular circles sharing center and radius -> a sphere
    #   - two circles connected by lines -> a cyllinder
    #   - any number of lines forming closed faces
    #   - any number of closed faces forming closed shells
    return transform_shape(orient, drawcalls_to_shape(comp.drawcalls))


def instrument_to_assembly(inst):
    from cadquery import Assembly
    a = Assembly()
    for component in inst.components:
        a= a.add(component_to_brep(component), name = component.name)
    return a


def main(instr=None, dirname=None, **kwds):
    from logging import basicConfig, INFO
    basicConfig(level=INFO)

    # output directory
    if dirname is None:
        from datetime import datetime as dt
        p = Path(instr).resolve()
        dirname = str(p.parent.joinpath(f"{p.stem}_{dt.strftime(dt.now(), '%Y%m%d_%H%M%S')}"))

    from mccodelib.mcdisplayutils import McDisplayReader
    reader = McDisplayReader(dir=dirname, instr=instr, **kwds)
    instrument = reader.read_instrument()
    
    # The reader fails if this is done before reader.read_instrument() above ?!
    root = Path(dirname)
    root.mkdir(parents=True, exist_ok=True)
    
    # Do the conversion to a CadQuery assembly, then save it to the desired location
    assembly = instrument_to_assembly(instrument)
    assembly.save(str(root.joinpath(f"{Path(instr).stem}.step")))


if __name__ == '__main__':
    from mccodelib.mcdisplayutils import make_common_parser
    # Only pre-sets instr, --default, options
    parser, prefix = make_common_parser(__file__, __doc__)

    parser.add_argument('--dirname', help='output directory name override')
    parser.add_argument('-n', '--ncount', dest='n', type=int, default=0, help='Number of particles to simulate')

    args, unknown = parser.parse_known_args()
    # if --inspect --first or --last are given after instr, the remaining args become "unknown",
    # but we assume that they are instr_options
    args = {k: args.__getattribute__(k) for k in dir(args) if k[0] != '_'}
    if len(unknown):
        args['options'] = unknown

    main(**args)


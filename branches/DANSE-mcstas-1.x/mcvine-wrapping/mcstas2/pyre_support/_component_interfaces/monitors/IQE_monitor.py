#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                        (C) 2008  All Rights Reserved
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#



from default import ComponentInterface as base

class ComponentInterface(base):

    def _get_histogram(self):
        return get_histogram(self)

    
    def _normalizeHistogram(self, histogram):
        norm = getNormalization(self, N=1000000)
        return histogram/norm
    

def get_histogram( monitor ):
    from mcstas2.utils.carray import bpptr2npyarr
    core = monitor.core()

    nQ = core.nQ; nE =core.nE
    n = nQ * nE
    shape = nQ, nE

    Iarr = bpptr2npyarr( core.getIQE_p( ), 'double', n ).copy()
    E2arr = bpptr2npyarr( core.getIQE_p2( ), 'double', n ).copy()
    Iarr.shape = E2arr.shape = shape

    from histogram import histogram, axis, arange
    dE = (core.Emax-core.Emin)/nE
    Eaxis = axis( 'energy', arange( core.Emin, core.Emax, dE ), unit = 'meV' )

    dQ = (core.Qmax-core.Qmin)/nQ
    Qaxis = axis( 'Q', arange( core.Qmin, core.Qmax, dQ ), unit = 'angstrom**-1' )

    h = histogram( 'I(Q,E)', [Qaxis,Eaxis], data = Iarr, errors = E2arr )
    return h


def getNormalization(monitor, N=1000000, epsilon=1e-7):
    # randomly shoot neutrons to monitor in 4pi solid angle
    
    core = monitor.core()
    import mcni, random, mcni.utils.conversion as conversion, math, os
    
    # 1. create neutrons
    neutrons = mcni.neutron_buffer(N)
    vi = conversion.e2v(core.Ei)
    for i in range(N):
        # randomly select E, the energy transfer
        E = core.Emin + random.random() * (core.Emax-core.Emin)
        # the final energy
        Ef = core.Ei - E
        # the final velocity
        vf = conversion.e2v(Ef)
        # choose theta
        theta = random.random() * math.pi
        # choose phi
        phi = random.random() * math.pi * 2.
        # compute final velocity vector
        vfv = vf*math.sin(theta)*math.cos(phi), vf*math.sin(theta)*math.sin(phi), vf*math.cos(theta)
        # prob
        prob = math.sin(theta) * (vf/vi)
        # the neutron
        neutrons[i] = mcni.neutron(prob=prob, v=vfv)
        continue
    
    # 2. create a copy of the original monitor
    from mcstas2 import componentfactory
    cf = componentfactory(type='IQE_monitor', category='monitors')
    props = [
        'Ei',
        'Emin', 'Emax', 'nE',
        'Qmin', 'Qmax', 'nQ',
        'max_angle_out_of_plane', 'min_angle_out_of_plane',
        'max_angle_in_plane', 'min_angle_in_plane',
        ]
    kwds = {}
    for p in props: kwds[p] = getattr(core, p)
    import tempfile
    tmpdir = tempfile.mkdtemp()
    outfilename = os.path.join(tmpdir, 'mon.dat')
    kwds['filename'] = outfilename
    monitorcopy = cf('monitor', **kwds)
    
    # 3. send neutrons to monitor copy
    monitorcopy.process(neutrons)
    h = get_histogram(monitorcopy)
    # for debug
    # import histogram.hdf as hh
    # hh.dump(h, 'tmp.h5', '/', 'c')
    h.I[h.I<epsilon] = 1
    return h


def test1():
    from mcstas2 import componentfactory
    cf = componentfactory(type='IQE_monitor', category='monitors')
    monitor = cf('monitor')
    getNormalization(monitor, N=1000000)
    return


def main():
    test1()
    return


if __name__ == '__main__': main()


# version
__id__ = "$Id$"


# End of file 

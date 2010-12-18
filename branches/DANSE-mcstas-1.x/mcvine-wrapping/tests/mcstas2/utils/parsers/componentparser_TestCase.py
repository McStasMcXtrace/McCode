#!/usr/bin/env python
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                                   Jiao Lin
#                      California Institute of Technology
#                        (C) 2007 All Rights Reserved  
#
# {LicenseText}
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#



import unittestX as unittest
import journal


class TestCase(unittest.TestCase):

    def test(self):
        "parse E_monitor component"
        from mcstas2.utils.parsers import parseComponent
        component = parseComponent( 'E_monitor.comp' )
        print dir(component)

        self.assertEqual(component.name, 'E_monitor')
        self.assert_(component.copyright.startswith('Written by: Kristian'))
        self.assertEqual(
            component.simple_description,
            'Energy-sensitive monitor.')
        self.assert_(
            component.full_description.strip().startswith(
                'A square single monitor'
                )
            )
        ips = component.input_parameters
        ips0 = ips[0]
        self.assertEqual(ips0.name, 'name')
        self.assertEqual(ips0.type, 'char *')
        self.assertEqual(ips0.default, 'e_monitor')
        ips1 = ips[1]
        self.assertEqual(ips1.name, 'nchan')
        self.assertEqual(ips1.type, 'int')
        self.assertEqual(ips1.default, 20)
        ips2 = ips[2]
        self.assertEqual(ips2.name, 'filename')
        self.assertEqual(ips2.type, 'char *')
        self.assertEqual(ips2.default, 'e.dat')
        ips3 = ips[3]
        self.assertEqual(ips3.name, 'xmin')
        self.assertEqual(ips3.type, 'double')
        self.assertAlmostEqual(ips3.default, -0.2)

        self.assertEqual(
            component.state_parameters,
            ['x', 'y', 'z', 'vx', 'vy', 'vz', 't', 's1', 's2', 'p']
            )
        self.assertEqual(
            component.output_parameters,
            []
            )
        self.assertEqual(
            component.declare.strip(),
            '{ double *E_N, *E_p, *E_p2; }',
            )
        self.assertEqual(
            component.initialize.strip(),
'''{
    int i;
    E_N = (double *)malloc(nchan*sizeof(double));
    E_p = (double *)malloc(nchan*sizeof(double));
    E_p2 = (double *)malloc(nchan*sizeof(double));

    for (i=0; i<nchan; i++)
    {
      E_N[i] = 0;
      E_p[i] = 0;
      E_p2[i] = 0;
    }
  }''')
        self.assertEqual(
            component.trace.strip(),
'''{
    int i;
    double E;

    PROP_Z0;
    if (x>xmin && x<xmax && y>ymin && y<ymax)
    {
      E = VS2E*(vx*vx + vy*vy + vz*vz);

      i = floor((E-Emin)*nchan/(Emax-Emin));
      if(i >= 0 && i < nchan)
      {
        E_N[i]++;
        E_p[i] += p;
        E_p2[i] += p*p;
        SCATTER;
      }
    }
  }''')
        self.assertLinesEqual(
            component.save.strip(),
'''{
#ifdef DEBUG
  printf("E_monitor: save\\n");
#endif
    DETECTOR_OUT_1D(
        "Energy monitor",
        "Energy [meV]",
        "Intensity",
        "E", Emin, Emax, nchan,
        &E_N[0],&E_p[0],&E_p2[0],
        filename);
  }''')
        self.assertLinesEqual(
            component.finalize.strip(),
'''{
#ifdef DEBUG
printf("free: E_N = %p, E_p = %p, E_p2 = %p\\n", E_N, E_p, E_p2);
#endif
free(E_N); free(E_p); free(E_p2);
}''')
        return


    def assertLinesEqual(self, left, right):
        left = left.splitlines()
        right = right.splitlines()
        if len(left) != len(right):
            raise AssertionError, "# of lines differ: %s vs %s" % (
                len(left), len(right))
        for l, r in zip(left, right):
            self.assertEqual(l, r)
            continue
        return


    pass  # end of TestCase



def pysuite():
    suite1 = unittest.makeSuite(TestCase)
    return unittest.TestSuite( (suite1,) )


def main():
    #debug.activate()
    #journal.debug("CompositeNeutronScatterer_Impl").activate()
    pytests = pysuite()
    alltests = unittest.TestSuite( (pytests, ) )
    unittest.TextTestRunner(verbosity=2).run(alltests)
    
    
if __name__ == "__main__":
    main()
    
# version
__id__ = "$Id$"

# End of file 

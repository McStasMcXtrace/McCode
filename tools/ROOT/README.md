# Rudimentary use of the root-mccode interface
This is work in progress - hence the interface may change at some point in time.

Start by loading the rootmccode-interface from inside the root-prompt. E.g.:
<code>
.L /path/to/module/rootmccode.C
</code>

Next we create a run-object. Assuming that we have a copy of BNL_H8.instr in the current directory.
<code>
 auto r = Mccoderun('BNL_H8.instr');
 r. set_flavour('mcstas');
</code>

N.b. It is possible to the output directory, ncount, etc. by directly setting the data membr fields of the run object.

Once we have an object we may proceed to run the simulation
<code>
 r.run()
</code>
or
<code>
r.generate_c()
r.exec()
</code>

Assuming all turned out well this will have run a simulation in the current working
directory - we can now produce an overview plot by means of
<code>
auto p = Mcplot()
p.plot()
</code>

Alternatively we may call a helper method directly to create a histogram object
<code>
auto h1 = mccode_plot_hist("D0_Source.dat","D0 Source", 1.0, 1.0)
h1-Draw()
</code>

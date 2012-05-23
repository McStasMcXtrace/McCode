#!/usr/bin/env python

from app import db_session
from models import *
from subprocess import Popen, PIPE
from os.path import basename, dirname, splitext

import time, os, shutil, re, json
import traceback


SIM_SRC_PATH = "sim/src/%s.instr"
SIM_BIN_PATH = "sim/src/%s.out"

WORK_PATH = "out/%s"


# try to use new R plotter instead of mcplot
def mcplot(simfile, outfile, logy=False):
    ''' Plot a mcstas.sim file with mcplot '''
    pid = Popen(["mcplot", "-gif"] +
                (logy and ["-log"] or []) +
                [basename(simfile)],
                cwd=dirname(simfile))
    pid.communicate()
    print simfile, outfile
    os.rename("%s.gif" % simfile , outfile)
try:
    from rplot.plot import plotSim
    def plot(simfile, outfile, logy=False):
        ''' Plot a sim file with R '''
        try:
            plotSim(simfile, logy, 'png')
            os.rename('%s.png' % simfile, outfile)
        except Exception,e:
            print e
            mcplot(simfile, outfile, logy)
    print 'using plotter from rplot/'
except Exception, e:
    print e
    plot = mcplot


def display(instr, params, outfile, fmt="gif"):
    ''' Display instrument '''
    pid = Popen(["mcdisplay", "-k", "--save", "-"+fmt,
                 basename(instr),
                 "-n", str(1) # precision (iterations)
                 ] + params,
                cwd=dirname(instr))
    print outfile
    (out, err) = pid.communicate()
    if err: print err
    os.rename(splitext(instr)[0] + ".out."+fmt, outfile)


def work():
    ''' Process a job by running the McStas simulation '''

    # fetch job
    run = SimRun.query.filter_by(status="waiting").order_by('created').first()
    if run is None:
        return

    run.status = "running"
    db_session.commit()

    print "Running job: ", run.id
    params = run.params

    # create output folder, this works as a lock
    workdir = "%s/%%s" % (WORK_PATH % run.id)
    try:
        os.mkdir(workdir % "")
    except OSError:
        # Someone else beat us to it, bail out
        print "Skipping: already running."
        return

    # pick seed and samples
    seed = params["_seed"]
    samples = params["_samples"]
    del params["_seed"]
    del params["_samples"]

    # instrument name
    name = run.sim.name
    siminstr = SIM_SRC_PATH % name
    simbin = SIM_BIN_PATH % name

    # generate list of parameters
    params = [ '%s=%s' % (str(k),str(v)) for k,v in params.items() ]

    # copy instrument file
    shutil.copy(siminstr, workdir % (name + ".instr"))
    shutil.copy(simbin, workdir % (name + ".out"))

    # compute instrument layout
    display(workdir % (name + ".instr"), params, workdir % "layout.gif")

    # run mcstas via mcrun
    pid = Popen(["mcrun"] +
                (seed > 0 and ["--seed", str(seed)] or []) +
                ["--ncount", str(samples),
                 "--dir", workdir % "mcstas",
                 siminstr] + params,
                stdout=PIPE,
                stderr=PIPE)
    (out, err) = pid.communicate()

    # debug
    if err: print err

    # populate result folder
    file(workdir % "out.txt", "w").write(out)
    file(workdir % "err.txt", "w").write(err)

    # dump components
    comps = re.findall(r'filename:\s*([^\s]+)',
                       file(workdir % 'mcstas/mcstas.sim').read())
    file(workdir % "comps.json", "w").write(json.dumps(comps))
    # plot components
    for comp in comps:
        for mode in ("lin", "log"):
            plot(workdir % "mcstas/" + comp,
                 outfile=workdir % ('plot-%s-%s.gif' % (comp, mode)),
                 logy=(mode == "log"))

    run.status = "done"
    db_session.commit()

    print "Done."


if __name__ == '__main__':
    while True:
        time.sleep(1)
        try:
            work()
        except:
            traceback.print_exc()

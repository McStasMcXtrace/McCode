#!/usr/bin/env python

from models import *
from subprocess import Popen, PIPE
import time, os

SIM_PATH = "sim/src/%s.instr"

WORK_PATH = "out/%s"

def work():
    # fetch job
    run = SimRun.query.filter_by(status="waiting").order_by('created').first()
    if run is None:
        return

    run.status = "running"
    db.session.commit()

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

    # locate binary
    name = run.sim.name
    simbin = SIM_PATH % name
    fid = Popen(["mcrun"] +
                (seed > 0 and ["--seed", str(seed)] or []) +
                ["--ncount", str(samples),
                 "--dir", workdir % "mcstas",
                 simbin]
                # parameters (NAME=VALUE)
                + [ '%s=%s' % (str(k),str(v)) for k,v in params.items() ],
                stdout=PIPE,
                stderr=PIPE)
    (out, err) = fid.communicate()

    # populate result folder
    file(workdir % "out", "w").write(out)
    file(workdir % "err", "w").write(err)

    run.status = "done"
    db.session.commit()

    print "Done."


if __name__ == '__main__':
    while True:
        time.sleep(1)
        try:
            work()
        except Exception,e:
            print e

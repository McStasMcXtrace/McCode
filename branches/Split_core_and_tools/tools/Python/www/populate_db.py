from app import db, db_session
from models import Simulation, Param, ParamDefault

from os.path import basename
from commands import getstatusoutput
from glob import glob

import json
import re

PATH_BIN = 'sim'
RE_types   = re.compile(r'(\w+)\((\w+)\)')
RE_default = re.compile(r'\s*Param:\s+(\w+)=(.*)')


def fetch(model, **kwargs):
    return model.query.filter_by(**kwargs)

def exist(model, **kwargs):
    return fetch(model, **kwargs).count() > 0


def info(bin):
    # Insert new simulation
    sim_name = basename(bin)[:-1*len('.out')]
    sim = None
    if exist(Simulation, name=sim_name):
        print 'Updating existing simulation: ' + sim_name
        sim = fetch(Simulation, name=sim_name)[0]
    else:
        # Create new simulation
        sim = Simulation(name=sim_name)
        db_session.add(sim)
        db_session.commit()

    # Delete old params
    print sim.id
    ParamDefault.query.filter_by(sim_id=sim.id).delete()
    # db_session.delete(param)
    db_session.commit()

    # Get info from executable
    status, out = getstatusoutput('%s --info' % bin)
    if status != 0:
        raise RuntimeWarning('Could not get info from %s' % bin)
    lines = out.split('\n')

    # Extract types and defaults
    types    = dict(RE_types.findall(filter(lambda x: 'Parameters:' in x, lines)[0]))
    defaults = {}
    map(lambda s: defaults.update(dict([RE_default.match(s).groups()])),
        filter(lambda x: 'Param:' in x, lines))

    # Combine types and defaults
    params = dict((p, (types[p], defaults[p])) for p in types)

    # Insert new params
    for param in sorted(params):
        if not exist(Param, name=param):
            print 'new param: ', param
            p = Param(name=param)
            db_session.add(p)
            db_session.commit()
        else:
            [p] = fetch(Param, name=param)
        # Insert param default
        convertfns = {'string' : lambda x: x,
                      'double' : float
                    }
        f = convertfns[types[param]]

        db_session.add(
            ParamDefault(param=p, value=f(defaults[param]), sim=sim))

    # Commit work so far
    db_session.commit()


def getlist():
    return glob(PATH_BIN + '/*.out')


if __name__ == '__main__':
    print "* Populating database with executables from " + PATH_BIN
    map(info, getlist())

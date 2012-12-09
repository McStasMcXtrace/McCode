from app import db, db_session
from models import Simulation, Param

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


def read_params(instr_file):
    simf = file(instr_file)

    # Skip lines up to parameters
    for line in simf:
        if line.lstrip('*').strip().lower().startswith('%parameters'):
            break

    # Read parameters
    params = {}
    for line in simf:
        line = line.lstrip('*').strip()
        match = re.match(r'([^:]+):\s*\[([^\]]*)\]\s*(.*)$', line)
        if not match:
            break
        name, unit, msg = match.groups()
        params[name] = (unit, msg)

    return params


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
    print sim.id, sim_name

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

    # Extract unit information and description messages
    info = read_params('%s.instr' % bin[:-1*len('.out')])

    # Combine types and defaults
    # params = dict((p, (types[p], defaults[p])) for p in types)

    # Insert param default
    convertfns = {'string' : lambda x: x,
                  'double' : float
                  }

    # Insert new params
    for param in sorted(types.keys()):
        if not exist(Param, sim_id=sim.id, name=param):
            print 'new param: ', param
            p = Param(sim=sim, name=param)
            db_session.add(p)
            db_session.commit()
        else:
            [p] = fetch(Param, name=param)

        unit, msg = info[param]
        p.unit = unit
        p.msg = msg
        p.default_value = convertfns[types[param]](defaults[param])

    # Commit work so far
    db_session.commit()


def getlist():
    return glob(PATH_BIN + '/*.out')


if __name__ == '__main__':
    print "* Populating database with executables from " + PATH_BIN
    map(info, getlist())

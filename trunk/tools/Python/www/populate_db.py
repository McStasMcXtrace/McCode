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
    priority = 0
    for line in simf:
        # Leave some space for later adjustment (defines order on webpage)
        priority += 10
        # Extract parameter
        line = line.lstrip('*').strip()
        if 'INPUT PARAMETERS' in line.upper():
            continue

        if line.startswith('%'):
            break

        parS = r'(\[|\()'  # [ or (
        parE = r'(\]|\))'  # ] or )
        match0 = re.match(r'(?P<name>[^\s:]+)\s*:?\s*'+parS+'(?P<unit>[^\]\)]*)'+parE+'(?P<msg>.*)$', line)
        match1 = re.match(r'(?P<name>[^\s:]+)\s*:?(?P<msg>.*)'+parS+'(?P<unit>[^\]\)]*)'+parE+'\s*$', line)
        match2 = re.match(r'(?P<name>[^\s:]+)\s*:?(?P<msg>.*)$', line)
        match = match0 or match1 or match2
        if not match:
            continue

        groups = match.groupdict()
        name = groups.get('name', '')
        unit = groups.get('unit', '?')
        msg  = groups.get('msg',  '?')
        if name:
            params[name.strip().lower()] = (priority, unit.strip(), msg.strip())

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

    # Extract priority, unit information and description messages
    infos = read_params('%s.instr' % bin[:-1*len('.out')])

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
            [p] = fetch(Param, sim_id=sim.id, name=param)

        if param.lower() in infos:
            priority, unit, msg = infos[param.lower()]
        else:
            priority = 100000
            unit, msg = '??'

        p.priority = priority
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

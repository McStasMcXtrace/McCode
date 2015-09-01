#================================#
# Editor: Mark Lewis             #
#================================#
# python imports
from os.path import basename, dirname
from commands import getstatusoutput
from glob import glob
import json
import re
# django imports
from django.core.management.base import NoArgsCommand
from optparse import make_option
from django.contrib.auth.models import User,Group
# app imports
from mcsimulator.models import Simulation
# consts
PATH_BIN = 'sim'
RE_types   = re.compile(r'(\w+)\((\w+)\)')                # RE_types  : string(string)
RE_default = re.compile(r'\s*Param:\s+(\w+)=(.*)')        # RE_default: 'Param:' string'='(string != \n) 
DEFAULT_UNIT = '?'
DEFAULT_MSG  = 'Parameter missing description.'

#================#
# Helper Methods #
#================#
#--------------------------#
# sqlite DB access methods #
#--------------------------#
def fetch(model, **kwargs):
    return model.objects.filter(**kwargs)
def exist(model, **kwargs):
    return fetch(model, **kwargs).count() > 0
#-----------------#
# listing????     #
#-----------------#
def getlist():
    return glob(PATH_BIN + '/*/*.out')
def main():
    map(info, getlist())

#=======================#
# DB Population methods #
#=======================#
#------------------------------------------------------#
# read_params                                          #
# -----------                                          #
# - Param Dictionary created from input instr file:    #
#          params[<name>] = (priority, <unit>, <msg>)  #
#   where: priority is assigned in the loop            #
#          <kwd> are read from instr file.             #
# nb. match objects are pretty nice.                   #
#------------------------------------------------------#
def read_params(instr_file):                
    simf = file(instr_file)
    # Skip lines up to parameters
    for line in simf:
        if line.lstrip('*').strip().lower().startswith('%parameters'): break
    # Read parameters
    params = {}
    priority = 0
    for line in simf:
        priority += 10
        # Extract parameter
        line = line.lstrip('*').strip()
        if 'INPUT PARAMETERS' in line.upper(): continue
        if line.startswith('%'): break
        parS = r'(\[|\()'  # [ or (
        parE = r'(\]|\))'  # ] or )
        match0 = re.match(r'(?P<name>[^\s:]+)\s*:?\s*'+parS+'(?P<unit>[^\]\)]*)'+parE+'(?P<msg>.*)$', line)
        match1 = re.match(r'(?P<name>[^\s:]+)\s*:?(?P<msg>.*)'+parS+'(?P<unit>[^\]\)]*)'+parE+'\s*$', line)
        match2 = re.match(r'(?P<name>[^\s:]+)\s*:?(?P<msg>.*)$', line)
        match = match0 or match1 or match2
        if not match: continue
        # build dict from matched line
        groups = match.groupdict()
        name = groups.get('name', '')
        unit = groups.get('unit', DEFAULT_UNIT)
        msg  = groups.get('msg',  DEFAULT_MSG)
        # put param into params dict
        if name: params[name.strip().lower()] = (priority, unit.strip(), msg.strip())
    return params
#--------------------------------------------------------------------------------------#
# build_params                                                                         #
# ------------                                                                         #
# (a) Extracting types:                                                                #
#      get list entry beginning 'Parameters:'                                          #
#      find each contiguous string corresponding to RE_types (defined at top)          #
#      make a dict from these thus:          # key and val are strings.                #
#        types[<param_name>] = <param_type>                                            #
# (b) Extract priority, unit information and description messages                      #
#      call read_params                                                                #
# (c) Extracting defaults:                                                             #
#      take the lines containing 'Param:'                                              #
#      map each resultant line to                                                      #
#        remove the 'Param:' and '=' by pattern matching on RE_default (defined above) #
#        make a dict from the list (<name>, <default val>)                             #
#        update the defaults dict with this entry                                      #
# (d) build params dict from parsed file and return it.                                #
#--------------------------------------------------------------------------------------#
def build_params(bin):
    # Parameter type caster using constructors
    convertfns = {'string': lambda x: x, 'double': float, 'int': int}
    # Get info from executable
    status, out = getstatusoutput('%s --info' % bin)
    if status != 0: raise RuntimeWarning('Could not get info from %s' % bin)
    lines    = out.split('\n') 
    types    = dict(RE_types.findall(filter(lambda x: 'Parameters:' in x, lines)[0])) # (a)
    infos    = read_params('%s.instr' % bin[:-1*len('.out')])                         # (b)
    defaults = {}
    params   = {}
    map(lambda s: defaults.update(dict([RE_default.match(s).groups()])),
        filter(lambda x: 'Param:' in x, lines))                                       # (c)
    for param in sorted(types.keys()):                                                # (d)
        priority, unit, msg = infos[param.lower()]
        try: params[param] = (priority,
                              convertfns[types[param]](defaults[param]),
                              unit, msg)
        except KeyError: params[param] = "NO DEFAULT DEFINED"
    return params
#--------------------------------------------------#
# info                                             #
# ----                                             #
# - makes a simulation model and puts it in the    #
#   Simulation DB table                            #
#--------------------------------------------------#
def info(bin):
    # Insert new simulation
    sim_name = basename(bin)[:-1*len('.out')]
    sim_group = basename(dirname(bin))
    Group.objects.get_or_create(name=sim_group)
    sim_id = sim_group+'_'+sim_name
    sim = None

    if exist(Simulation, name=sim_id, simgroup=sim_group, displayname=sim_name):
        print 'Updating existing simulation: ' + sim_name
        sim = fetch(Simulation, name=sim_id)[0]
    else:
        print "Inserting new simulation: %s"%sim_name
        sim = Simulation(name=sim_id, simgroup=sim_group, displayname=sim_name,
                         params=build_params(bin))
        sim.save()
    print "db_id: %s\n sim_id: %s\n simulalation: %s "%(sim.id, sim_id, sim_name)
#======================#
# Command line binding #
#======================#
class Command(NoArgsCommand):
    help = "Whatever you want to print here"
    option_list = NoArgsCommand.option_list + (
        make_option('--verbose', action='store_true'),
    )
    def handle_noargs(self, **options):
        main()

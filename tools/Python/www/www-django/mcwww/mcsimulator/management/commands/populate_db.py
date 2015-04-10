#================================#
# Being Modified to account for  #
# the parameter reimplementation #
# Editor: Mark Lewis             #
#================================#
# python imports
from os.path import basename, dirname
from commands import getstatusoutput
from glob import glob
import json
import re
# django imports
from django.core.management.base import NoArgsCommand, make_option
from django.contrib.auth.models import User,Group
# app imports
from mcsimulator.models import Simulation, Param
# consts
PATH_BIN = 'sim'
RE_types   = re.compile(r'(\w+)\((\w+)\)')                # RE_types  : string(string)
RE_default = re.compile(r'\s*Param:\s+(\w+)=(.*)')        # RE_default: 'Param:' string'='(string != \n) 
DEFAULT_UNIT = '?'                                        # PARAMS
DEFAULT_MSG  = 'Parameter missing description.'           # PARAMS

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
        if line.lstrip('*').strip().lower().startswith('%parameters'):
            break
    # Read parameters
    params = {}
    priority = 0
    for line in simf:
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
        # build dict from matched line
        groups = match.groupdict()
        name = groups.get('name', '')
        unit = groups.get('unit', DEFAULT_UNIT)
        msg  = groups.get('msg',  DEFAULT_MSG)
        # put param into params dict
        if name:
            params[name.strip().lower()] = (priority, unit.strip(), msg.strip())
    return params
#--------------------------------------------------#
# info
# ----
# - 
#
#--------------------------------------------------#
def info(bin):
    #----------------------------------------------------------------------------------------------#
    # SIMULATION CREATION
    # Simulation needs to be created and stored AFTER params are pulled from the instr file
    #
    # Insert new simulation
    sim_name = basename(bin)[:-1*len('.out')]
    sim_group = basename(dirname(bin))
    Group.objects.get_or_create(name=sim_group)
    full_sim_name = sim_group+'/'+sim_name
    sim = None
    if exist(Simulation, name=sim_name, simgroup=sim_group, displayname=sim_name):
        print 'Updating existing simulation: ' + sim_name
        sim = fetch(Simulation, name=sim_name)[0]
    else:
        # Create new simulation
#        sim = Simulation(name=sim_group+"_"+sim_name, simgroup=sim_group, displayname=sim_name) # PUT PARAMS DICT IN HERE AS NEW JSONFIELD
        sim = Simulation(name=sim_group+"_"+sim_name, simgroup=sim_group, displayname=sim_name, params={})
        sim.save()
    # Delete old params
    print sim.id, sim_name
    # Get info from executable
    status, out = getstatusoutput('%s --info' % bin)
    if status != 0:
        raise RuntimeWarning('Could not get info from %s' % bin)
    lines = out.split('\n')      # gives a list of strings (try this by compiling an instrument file and running ./<blah>.out --info, it will show you the output)
    # END SIMULATION CREATION
    #----------------------------------------------------------------------------------------------#

    #----------------------------------------------------------------------------------------------#
    # PARAMETER ASSIGNMENT => PARAMS DICT CREATION - I think someone was taking a course in functional programming when they wrote this bit :p
    # - must extend the params dict returned by read_params to have a 'value' entry in the tuple.
    #   - maybe move the reading of the defaults into read_params
    #   - have to match the name with the key... where do default values come from?
    #
    # (a) Extracting types: 
    #   get list entry beginning 'Parameters:'
    #   find each contiguous string corresponding to RE_types (defined at top)
    #   make a dict from these thus:          # key and val are strings.
    #      types[<param_name>] = <param_type>
    #
    # (b) Extracting defaults:
    #   take the lines containing 'Param:'
    #   map each resultant line to
    #     remove the 'Param:' and '=' by pattern matching on RE_default (defined above)
    #     make a dict from the list (<name>, <default val>)
    #     update the defaults dict with this entry
    #
    # (c) Extract priority, unit information and description messages
    #   call read_params
    # 
    # (d) Insert Param to Param DB - Obviously this one has to change to put dict into Simulation DB
    #    
    #
    # (a)
    types    = dict(RE_types.findall(filter(lambda x: 'Parameters:' in x, lines)[0])) # this is a lovely line of code
    # (b)
    defaults = {}
    map(lambda s: defaults.update(dict([RE_default.match(s).groups()])),
        filter(lambda x: 'Param:' in x, lines))                                       # man, this whole chunk is nice
    # (c)
    infos = read_params('%s.instr' % bin[:-1*len('.out')])                            # concise way of removing the extension
    # Parameter type caster using constructors
    convertfns = {'string': lambda x: x, 'double': float, 'int': int}
    # (d)
    for param in sorted(types.keys()):
        if not exist(Param, sim_id=sim.id, name=param):
            print 'new param: ', param
            p = Param(sim=sim, name=param)
        else:
            p = fetch(Param, sim_id=sim.id, name=param)[0]
        if param.lower() in infos:
            priority, unit, msg = infos[param.lower()]
        else:
            priority = 100000
            unit = DEFAULT_UNIT
            msg  = DEFAULT_MSG
        p.priority = priority
        p.unit = unit
        p.msg = msg
        # cast to correct type
        p.default_value = convertfns[types[param]](defaults[param])
        p.save()
    #----------------------------------------------------------------------------------------------#
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

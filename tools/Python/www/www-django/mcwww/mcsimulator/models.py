#==========================================================#
# - Re-write of interplay between the                      #
#   - Parameters                                           #
#   - ParamValues                                          #
#   - Simulations                                          #
#   - Jobs                                                 #
#   - SimRun                                               #
#   objects. Want to remove both Param  DB objects and put #
#   as dict in other relevent objects.                     #
# - Reformulate the admin sites in                         #
#   admin.py                                               #
# ------------------                                       #
# Author: Mark Lewis                                       #
#==========================================================#
# python imports
from json import dumps, loads
from datetime import datetime
from jsonfield import JSONField       # this needs to be in the dependencies!!! BEEP! BEEP! DANGER WILL ROGERS!!
# django imports
from django.db import models
from django.db.models import related
from django.contrib import admin
from django.utils.timezone import now
# app imports
from mcwww import settings
from mcsimulator.models import *
#--------------------------------------#
# Job model                            #
# ---------                            #
# - refers to specific simulation      #
# - params: last used parameters       #
# - ref : Unique permanent reference   #
# - sim : Simulation Relation (1-to-1) #
# - created :  Date and time created   #
#--------------------------------------#
class Job(models.Model):
    ref     = models.CharField(max_length=64, db_index=True)
    sim     = models.ForeignKey('Simulation')
    created = models.DateTimeField(db_index=True, editable=False) 
    seed    = models.IntegerField()
    samples = models.IntegerField()
    npoints = models.IntegerField()
    params  = JSONField(null=True, blank=True)
    @staticmethod 
    def new(ref, sim, seed=0, samples=1000000, npoints=1):
        return Job(None, ref, seed, samples, npoints, sim.id, created=now())
    def sim_details(self):
        return ("Simulation [name, id]: [%s, %s]") % (self.sim.name, self.sim.id)
    def __unicode__(self):
        return "Job Details [ref, sim name]: [%s, %s]" % (self.ref, self.sim.name)
#------------------------------------------#
# Simulation Model                         #
# ----------------                         #
# - Refers to a particular simulation      #
#  file that can be run by memebers of the #
#  correct group (simgroup)                #
# #--------------------------------------# # 
# # Simulation Parameters                # #
# # - dict format:                       # #
# #     params[<name>] = (priority,      # #
# #                       default value, # # 
# #                       unit,          # # 
# #                       message)       # #
# #--------------------------------------# #
#------------------------------------------#
class Simulation(models.Model):
    # Simulation name (see Param/sim.param_set() for parameters)
    name = models.CharField(max_length=64, unique=True, db_index=True)
    simgroup = models.CharField(max_length=64, unique=False, db_index=True)
    displayname = models.CharField(max_length=64, unique=False, db_index=True)
    params = JSONField(null=True, blank=True)
    def shortname(self):
        return self.displayname
    def __repr__(self):
        return self.name
    @staticmethod
    def new(*args, **kwargs):
        return Simulation(None, *args, **kwargs)
    def __unicode__(self):
        return u'<Sim %s>' % self.name
#---------------------------------------------------------#
# SimRun Model                                            #
# ------------                                            #
# - A particular run of a simulation (configured by Job)  #
# - ref : Unique reference (for use in urls)              #
# - created/completed : Dates for creation and completion #
# - status : Current status (waiting, running, completed) #
# - user, job, sim : foreign keys to other app models     #
#---------------------------------------------------------#
class SimRun(models.Model):
    ref = models.CharField(max_length=64, db_index=True)
    created   = models.DateTimeField(db_index=True, editable=False)
    completed = models.DateTimeField(db_index=True, editable=False, null=True)
    status = models.CharField(max_length=32, db_index=True)
    user = models.ForeignKey(settings.AUTH_USER_MODEL)
    job = models.ForeignKey('Job')
    sim = models.ForeignKey('Simulation')
    # Parameters and result as JSON - Do we need the str_params?
    str_params = models.TextField()
    str_result = models.TextField()
    @staticmethod
    def new(user, job, sim):
        ref = job.ref + "__" + str(datetime.now()).replace(' ', '_')
        run = SimRun(None, ref, user.id, job.id, sim.id,
                     'waiting', 'null', 'null', now(), None)
        run.params = sim.params
        run.result = None
        return run
    @property
    def params(self):
        return loads(self.str_params)
    @params.setter
    def params(self, p):
        self.str_params = dumps(p)
    @property
    def result(self):
        return loads(self.str_result)
    @result.setter
    def result(self, r):
        self.str_result = dumps(r)
    def __unicode__(self):
        return "SimRun Details [ref, job ref, sim name]: [%s, %s, %s]" % (self.ref, 
                                                                          self.job.ref, 
                                                                          self.sim.name)

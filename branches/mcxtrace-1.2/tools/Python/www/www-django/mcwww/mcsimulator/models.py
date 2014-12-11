# Mark Lewis - Edited 2014.
#
# Moved Admin classes to admin.py.y

from django.db import models
from django.db.models import related

from django.contrib import admin
from django.contrib.auth.models import User

from django.utils.timezone import now


from json import dumps, loads
from datetime import datetime


class Job(models.Model):
    ''' A job specifying the simuation to run and its status '''
    # Unique permanent reference (for use in links)
    ref = models.CharField(max_length=64, db_index=True)
    # Parameters
    seed    = models.IntegerField()
    samples = models.IntegerField()
    npoints = models.IntegerField()
    # Simulation to run
    sim     = models.ForeignKey('Simulation')
    # Date and time created
    created = models.DateTimeField(db_index=True, editable=False)
    @staticmethod
    def new(ref, sim, seed=0, samples=1000000, npoints=1):
        return Job(None, ref, seed, samples, npoints, sim.id, created=now())
    def sim_details(self):
        return ("Simulation [name, id]: [%s, %s]") % (self.sim.name, self.sim.id)
    def __unicode__(self):
        return "Job Details [ref, sim name]: [%s, %s]" % (self.ref, self.sim.name)

'''
class JobAdmin(admin.ModelAdmin):
    readonly_fields = ('created',)
'''

class Simulation(models.Model):
    ''' A simulation '''
    # Simulation name (see Param/sim.param_set() for parameters)
    name = models.CharField(max_length=64, unique=True, db_index=True)
    simgroup = models.CharField(max_length=64, unique=False, db_index=True)
    displayname = models.CharField(max_length=64, unique=False, db_index=True)
    # param_set()  gives the parameters
    # job_set()    gives the related jobs
    # simrun_set() gives the related runs
    def shortname(self):
        return self.displayname
    def __repr__(self):
        return self.name
    @staticmethod
    def new(*args, **kwargs):
        return Simulation(None, *args, **kwargs)
    def __unicode__(self):
        return u'<Sim %s>' % self.name


class SimRun(models.Model):
    ''' A particular run of a simulation (configured by Job) '''
    # Unique permanent reference (for use in links)
    ref = models.CharField(max_length=64, db_index=True)
    # Creator / owner
    user = models.ForeignKey(User)
    # Job to run
    job = models.ForeignKey('Job')
    sim = models.ForeignKey('Simulation')
    # Current status (waiting, running, completed)
    status = models.CharField(max_length=32, db_index=True)
    # Parameters and result as JSON
    str_params = models.TextField()
    str_result = models.TextField()
    # Dates for creation and completion
    created   = models.DateTimeField(db_index=True, editable=False)
    completed = models.DateTimeField(db_index=True, editable=False, null=True)
    @staticmethod
    def new(user, job, sim, params):
        ref = job.ref + "__" + str(datetime.now()).replace(' ', '_')
        run = SimRun(None, ref, user.id, job.id, sim.id,
                     'waiting', 'null', 'null', now(), None)
        run.params = params
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
        return "SimRun Details [ref, job ref, sim name]: [%s, %s, %s]" % (self.ref, self.job.ref, self.sim.name)

'''
class SimRunAdmin(admin.ModelAdmin):
    readonly_fields = ('created', 'completed')
'''

class Param(models.Model):
    ''' A parameter '''
    # Where to show the parameter in a listing (e.g. on configure page)
    priority = models.IntegerField()
    # Name, unit and help message
    name = models.CharField(max_length=128)
    unit = models.CharField(max_length=128)
    msg  = models.CharField(max_length=128)
    # JSON-encoded default value
    str_default = models.CharField(max_length=256)
    # The simulation this parameter belongs to
    sim = models.ForeignKey('Simulation')
    # value_set() gives the ParamValue's that use this parameter
    @staticmethod
    def new(sim, name, unit, value=None, msg=''):
        param = Param(None, sim.id, name, unit, msg, 'null')
        param.default_value = value
        return param
    def simulation(self):
        return Simulation.query.filter_by(id=self.sim_id).one()
    @property
    def default_value(self):
        return loads(self.str_default)
    @default_value.setter
    def default_value(self, val):
        self.str_default = dumps(val)
    def __unicode__(self):
        return '<Param: %s, %s %s>' % (self.name, self.default_value, self.unit)
'''
class ParamAdmin(admin.ModelAdmin):
    pass
'''

class ParamValue(models.Model):
    ''' A parameter value tied to a specific job '''
    param     = models.ForeignKey('Param')
    job       = models.ForeignKey('Job')
    str_value = models.TextField()
    @staticmethod
    def new(param, job, value):
        pv = ParamValue(None, param.id, job.id)
        pv.value = value
        return pv
    @property
    def value(self):
        return loads(self.str_value)
    @value.setter
    def value(self, val):
        self.str_value = dumps(val)
    def __unicode__(self):
        return self.param.name

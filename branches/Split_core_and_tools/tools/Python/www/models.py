from app import db
from sqlalchemy import DDL
from json import dumps, loads

class Job(db.Model):
    ''' A job specifying the simuation to run and its status '''
    id      = db.Column(db.String(38), primary_key=True)
    seed    = db.Column(db.Integer)
    samples = db.Column(db.Integer)
    sim_id  = db.Column(db.Integer, db.ForeignKey('simulation.id'))
    params  = db.relationship('ParamValue', backref='job')

    def __init__(self, id, seed, samples, sim):
        self.id = id
        self.seed = seed
        self.samples = samples
        self.sim_id = sim.id


class Simulation(db.Model):
    ''' A simulation '''
    id     = db.Column(db.Integer, primary_key=True, autoincrement=True)
    name   = db.Column(db.String(64))
    params = db.relationship('ParamDefault', backref='sim')
    jobs   = db.relationship('Job', backref='sim')

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name


class Param(db.Model):
    ''' A parameter '''
    id       = db.Column(db.Integer, primary_key=True)
    name     = db.Column(db.String(128))
    defaults = db.relationship('ParamDefault', backref='param')
    values   = db.relationship('ParamValue', backref='param')

    def __init__(self, name):
        self.name = name


class ParamDefault(db.Model):
    ''' A parameter default '''
    id        = db.Column(db.Integer, primary_key=True)
    param_id  = db.Column(db.Integer, db.ForeignKey('param.id'))
    str_value = db.Column(db.String(255))
    sim_id    = db.Column(db.ForeignKey('simulation.id'))

    def __init__(self, param, value, sim):
        self.param_id = param.id
        self.value    = value
        self.sim_id   = sim.id

    def simulation(self):
        return Simulation.query.filter_by(id=self.sim_id).one()

    @property
    def value(self):
        return loads(self.str_value)
    @value.setter
    def value(self, v):
        self.str_value = dumps(v)


class ParamValue(db.Model):
    ''' A parameter value tied to a specific job '''
    id        = db.Column(db.Integer, primary_key=True)
    param_id  = db.Column(db.Integer, db.ForeignKey('param.id'))
    str_value = db.Column(db.PickleType)
    job_id    = db.Column(db.ForeignKey('job.id'))

    def __init__(self, param, value, job):
        self.param_id = param.id
        self.value    = value
        self.job_id   = job.id

    def job(self):
        return

    @property
    def value(self):
        return loads(self.str_value)
    @value.setter
    def value(self, v):
        self.str_value = dumps(v)

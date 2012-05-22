#!/usr/bin/env python

# setup flask.ext for old versions of flask (<8)
import flaskext_compat
flaskext_compat.activate()


from flask import *
from util import skip, templated
from uuid import uuid4 as uuid
import sys

from app import app, db, db_session, SessionMaker
from models import Job, Simulation, SimRun, Param, ParamValue, ParamDefault


def convert_type(default, str_value):
    # tested types: str and float
    return type(default.value)(str_value)


@app.route('/')
def index():
    return redirect(url_for('configure', jobid=str(uuid())))

def get_sims():
    return Simulation.query.order_by('simulation.name').all()

@app.route('/job/<jobid>', methods=['GET'])
@templated()
def configure(jobid):
    job = Job.query.get(jobid)
    sims = get_sims()
    return dict(sims = sims, job=job, jobid=jobid)


@app.route('/job/update/<jobid>', methods=['POST'])
def configurePOST(jobid):
    oks    = []
    errors = []  # all ok
    def ok(name, old, f):
        ''' check parameter and update either oks or errors '''
        try:
            v = f()
            oks.extend([name])
            return v
        except:
            errors.extend([name])
            return old

    form = request.form
    sim = Simulation.query.filter_by(name=request.form['sim']).one()

    # defualts
    seed    = 0
    samples = 100000

    # lookup job
    query = Job.query.filter_by(id=jobid)
    job = None
    if query.count() == 0:
        # create job
        # TODO: check types of seed and samples
        job = Job(id=jobid, seed=seed, samples=samples, sim=sim)
        db.session.add(job)
    else:
        job = query.one()

    seed    = ok("seed",    seed,    lambda : abs(int(form['seed'])))
    samples = ok("samples", samples, lambda : abs(int(form['samples'])))

    # update job
    job.seed = seed
    job.samples = samples
    job.sim_id = sim.id

    # commit job
    db.session.commit()

    # insert / update params
    for name in skip(('sim', 'seed', 'samples'), form):
        str_value = form[name]
        param  = Param.query.filter_by(name=name).one()
        paramd = ParamDefault.query.filter_by(param_id=param.id, sim_id=sim.id).one()

        oldQ = ParamValue.query.filter_by(job_id=job.id, param_id=param.id)
        if oldQ.count() == 0:
            # no value set, use default as old
            old = paramd.value
        else:
            old = oldQ.one().value

        cvalue = ok(name, old, lambda : convert_type(paramd, str_value))

        valueQ = ParamValue.query.filter_by(job_id=job.id, param_id=param.id)
        if valueQ.count() == 0:
            # create parameter value
            pvalue = ParamValue(param=param, job=job, value=cvalue)
            db.session.add(pvalue)
        else:
            pvalue = valueQ.one()
            pvalue.value = cvalue
        # commit parameter value
        db.session.commit()

    return jsonify(errors=errors, oks=oks)


@app.route('/sim/<jobid>', methods=['GET'])
def simulate(jobid):
    ''' Create simulation job for the worker '''
    job = Job.query.filter_by(id=jobid).one()
    sim = Simulation.query.filter_by(id=job.sim_id).one()
    # treat seed and samples specially
    params = { "_seed": job.seed,
               "_samples": job.samples }
    # filter params by what the simulation expects (needed!)
    valid = set(pd.param.name for pd in sim.params)
    params.update(dict(
        (p.param.name, p.value) for p in job.params
        if p.param.name in valid))
    # create simulation run (for the worker to compute)
    run = SimRun(job=job, sim=sim, params=params)
    db.session.add(run)
    db.session.commit()
    # send user to status page
    return redirect(url_for('status', runid=run.id))


@app.route('/sim/status/<runid>', methods=['GET'])
@templated()
def status(runid):
    return dict(runid=runid)


if __name__ == '__main__':
    if '--init' in sys.argv[1:]:
        print 'Creating database..'
        db.init_app(app)
        db.create_all()
        sys.exit(0)
    app.run(debug=True)

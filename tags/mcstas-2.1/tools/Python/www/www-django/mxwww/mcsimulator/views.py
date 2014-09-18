from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User, Group

from django.shortcuts import redirect
from django.http import HttpResponse


from mcsimulator.models import *
from common import *

from mxwww.settings import STATIC_URL, MAX_RAY_SAMPLES, MAX_SCAN_POINTS


NONCE_NAME = 'csrfmiddlewaretoken'


def convert_type(default, str_value):
    # tested types: str and float
    if ',' in str_value:
        return map(lambda elem: convert_type(default, elem), str_value.split(','))
    return type(default)(str_value)


@login_required
@only_safe
def home(req):
    sim = get_or_404(Simulation.objects.all()[:1])
    job = Job.new(ref=new_key(), sim=sim)
    job.save()

    return redirect('configure', jobref=job.ref)


@login_required
@only_safe
@templated('mcsimulator/configure')
def configure(req, jobref):
    job = get_or_404(Job, ref=jobref)
    sims = Simulation.objects.all().filter(simgroup__in=req.user.groups.values_list('name',flat=True))
    return dict(job=job, jobid=job.ref, sims=sims, 
                max_samples=MAX_RAY_SAMPLES, max_npoints=MAX_SCAN_POINTS)

@login_required
@only_unsafe
def configurePOST(req, jobref):
    oks    = []
    errors = []  # all ok
    def ok(name, old, f):
        ''' check parameter and update either oks or errors '''
        try:
            v = f()
            oks.extend([name])
            return v
        except Exception,e:
            print e
            errors.extend([name])
            return old

    form = req.POST
    sim = get_or_404(Simulation, name=form['sim'])

    # defaults
    seed    = 0
    samples = 10^6
    npoints = 1

    # lookup job
    job = get_or_404(Job, ref=jobref)

    seed    = ok("seed",    seed,    lambda : abs(int(form['seed'])))
    samples = ok("samples", samples, lambda : abs(int(form['samples'])))
    npoints = ok("samples", samples, lambda : abs(int(form['npoints'])))

    # update job
    job.seed = seed
    job.samples = samples
    job.npoints = npoints
    job.sim_id = sim.id
    # save
    job.save()

    # insert / update params
    for name in skip((NONCE_NAME, 'sim', 'seed', 'samples', 'npoints'), form):
        str_value = form[name]
        param = get_or_404(Param, sim_id=sim.id, name=name)

        # lookup parameter value
        oldP = fetch1(ParamValue, job_id=job.id, param_id=param.id)

        # pick parameter value if present or use default
        if oldP is None:
            old = param.default_value
        else:
            old = oldP.value

        cvalue = ok(name, old, lambda : convert_type(param.default_value, str_value))

        pvalue = fetch1(ParamValue, job_id=job.id, param_id=param.id)
        if pvalue is None:
            # create parameter value
            pvalue = ParamValue.new(param=param, job=job, value=cvalue)

        # commit parameter value
        pvalue.value = cvalue
        pvalue.save()

    return jsonify(errors=errors, oks=oks)


@login_required
@only_unsafe
def simulatePOST(req, jobref):
    ''' Create simulation job for the worker '''
    job = get_or_404(Job, ref=jobref)
    # treat seed and samples specially
    params = { "_seed": job.seed,
               "_samples": job.samples,
               "_npoints": job.npoints
               }
    # filter params by what the simulation expects (needed!)
    valid = set(pd.name for pd in job.sim.param_set.all())
    params.update(dict(
        (pv.param.name, pv.value) for pv in job.paramvalue_set.all()
        if pv.param.name in valid))
    # create simulation run (for the worker to compute)
    run = SimRun.new(user=req.user, job=job, sim=job.sim, params=params)
    run.save()
    # send user to status page
    return redirect('status', runref=run.ref)


@only_safe
@templated('mcsimulator/status')
def status(req, runref, compN=None):
    run = get_or_404(SimRun, ref=runref)
    job = run.job

    params = sorted('%s=%s' % (p,v) for p,v in run.params.items()
                    if not p.startswith('_'))
    return dict(run = run, params=params,
                job = job, runref = runref, compN = compN)


@only_safe
def show_plot(req, runref, name):
    # remove tags, eg. plot-Lmon1.sim-lin.gif => Lmon1.sim
    mon = name.split('-', 1)[1].rsplit('-', 1)[0]
    prefix = os.path.dirname(name)
    # construct paths
    doc_url = '/static/out/%s/%s/%s' % (runref, prefix, mon)
    img_url = '/static/out/%s/%s' % (runref, name)
    return HttpResponse('<a href="%s"><img src="%s"></a>' % (doc_url, img_url))


@only_safe
def documentation(req, instr):
    return HttpResponse(file('sim/%s.html' % instr).read(),
                        content_type='text/html')


# @app.route('/sim/latest', methods=['GET'])
@login_required
@only_safe
def latest(req):
    ''' List latest simulations as JSON '''
    runs = fetch(SimRun, user=req.user)\
           .order_by('-created').all()[:25]

    return jsonify(runs=[{'id'        : run.id,
                          'runref'    : run.ref,
                          'time'      : run.created.strftime('%s'),
                          'instrument': run.sim.name}
                         for run in runs])

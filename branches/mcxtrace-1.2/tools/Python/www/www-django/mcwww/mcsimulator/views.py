# Edits: Mark Lewis 2014
# ======================
# 
# Method description comments. Formatted :

#  methodName()
# ==============
# how method called
#
# - how method works
#   (maybe some notes)
# - more functionality described
# - ...
#
# Also added some inline comments : 
''' comment ''' #  not with hash(pound), hash was the other guy.
#


#       Possible Job options        #
#       ====================        #

#  Nice to have:                    #
#   - actual home page with user    #
#     saved results.                #
#     (when save works)             #
                                   
#   - drop box to choose different  #
#     simulations.                  #
#     (available in DB)             #
#                                   #

#   - information and control of    #
#     logged on accounts and        #
#     running simulations from      #
#     admin accounts.               #

from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User, Group

from django.shortcuts import redirect
from django.http import HttpResponse


from mcsimulator.models import * # Will have to limit this import as going to build more DB objects that I don't want the std view to have access to.
from common import *

from mcwww.settings import STATIC_URL, MAX_RAY_SAMPLES, MAX_SCAN_POINTS

NONCE_NAME = 'csrfmiddlewaretoken'

#  convertType()
# ===============
# called from configurePOST to populate simulation parameters and 
# recursive, once, returns a list of typecast values
# 
# - test if the passed string is a ',' deliniated list ;
# - RECURSIVE CALL : return list of values describved below ;
# - return value contained in the string arg, typecast to default value type ;
#
def convert_type(default, str_value):
    # tested types: str and float
    if ',' in str_value:
        return map(lambda elem: convert_type(default, elem), str_value.split(','))
    return type(default)(str_value)

#  home()
# ========
# NEED TO FIND OUT HOW THIS METHOD IS CALLED - hard to track.
# ------------------------------------------
# ASSUMING: dictionary supplies the correct value of 
# 'next' to login.html upon authenticated login
# 
# - get first Simulation Model from Simulation DB ;
#   (populated w. default Simulations - ordering unknown)
# - create new Job Model (w. unique id etc.) containing default sim data ;
# - save Job Model in Job DB ;
#   access jobs via job.id, known to user as randomised, o/p stored in 'out' on server
# - redirect to simulation configuration page ;
#
@login_required
@only_safe
def home(req):
    sim = get_or_404(Simulation.objects.all()[:1])
    job = Job.new(ref=new_key(), sim=sim)
    job.save()

    return redirect('configure', jobref=job.ref)

#   configure()
#  =============
# redirected to by home()
#
# - get users Job from Job DB ;
#   job is either:
#         newly created by home()
#         configure.html 'Simulation selection' -> configue.js::switchLatest() -> req argument
# - sims is filled with the users history of simulation runs ;
#   this is used in configure.html to:
#         populate 'List latest sinulations' drop menu ;
#         populate default param_set ;
# - return the requested job id ;
#
@login_required
@only_safe
@templated('mcsimulator/configure')
def configure(req, jobref):
    job = get_or_404(Job, ref=jobref)
    sims = Simulation.objects.all().filter(simgroup__in=req.user.groups.values_list('name',flat=True))
    return dict(job=job, jobid=job.ref, sims=sims, 
                max_samples=MAX_RAY_SAMPLES, max_npoints=MAX_SCAN_POINTS)

#  configurePOST()
# =================
# configure.html form submission -> configure.js::run()
#
# - read in the posted form from the request and request the simulation ;
#   from the McsimulatorSimulation DB ;
# - setup simulation and job using user defined params, checking with ok() ;
# - save the job into the McsimulatorJob DB (jobref is passed as arg) ;
# - go through form, ignoring pre-setup params, storing the user defined parameters ;
# - return call to jsonify with tuple of list of erros and list of oks ;
#
@login_required
@only_unsafe
def configurePOST(req, jobref):
    oks    = []
    errors = []
    #  ok()
    # ======
    # scope : configurePOST()
    #
    # - if f() succeeds 
    #     add parameter name to oks list ;
    #     return f()'s return (set the parameter) ;
    # - if f() throws an exception (apparently could happen for all things not string or float)
    #     print the error to the console ;
    #     add parameter name to error list ;
    #     return the old (maybe default) value ;
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
    ''' requesting Simulation model match from database (common.py) '''
    sim = get_or_404(Simulation, name=form['sim'])

    # defaults
    seed    = 0
    samples = 10^6
    npoints = 1

    # lookup job
    ''' requesting Job model from database, should have been created in home() '''
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
        ''' query McsimulatorParam DB for 'name' '''
        param = get_or_404(Param, sim_id=sim.id, name=name)

        # lookup parameter value
        oldP = fetch1(ParamValue, job_id=job.id, param_id=param.id)

        # pick parameter value if present or use default
        if oldP is None:
            old = param.default_value
        else:
            old = oldP.value
        ''' check if the string contains a typecast-able value and store '''
        cvalue = ok(name, old, lambda : convert_type(param.default_value, str_value))

        pvalue = fetch1(ParamValue, job_id=job.id, param_id=param.id)
        if pvalue is None:
            # create parameter value
            pvalue = ParamValue.new(param=param, job=job, value=cvalue)

        # commit parameter value 
        ''' to McsimulatorParam DB '''
        pvalue.value = cvalue
        pvalue.save()

    return jsonify(errors=errors, oks=oks)


#  simulatePOST()
# ================
# called by configure.html -> configure.js::run()
# 
# - get the Job model from McsimulatorJob DB ;
# - set params with queries to McsimulatorJob model ;
# - filter:
#     valid = query McsimulatorJobs::McsimulatorSimulation DB to create set of parameter names ;
#     update the params dictionary (created above) using only those names that are stored in the valid set ;
# - build new SimRun model using user data and parameters and save to McSimulatorSimRun DB ;
# - return a redirect to status page (which will call the status method below) ;
#
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

#  status()
# ==========
# called by redirect to status page, simply provides the dictionary with which the html/js can 
# request simulation o/p files from the server. Most status update work done by js requests.
#
# - query to McsimulatorSimRun DB for users current run ;
# - create parameter dictionary from run.queries ;
# - return entire dictionary of run/job/parameter data ;
#
@only_safe
@templated('mcsimulator/status')
def status(req, runref, compN=None):
    run = get_or_404(SimRun, ref=runref)
    job = run.job

    params = sorted('%s=%s' % (p,v) for p,v in run.params.items()
                    if not p.startswith('_'))
    return dict(run = run, params=params,
                job = job, runref = runref, compN = compN)

#  showPlot()
# ============
# takes requests from status.js and returns urls (positions in the server dir structure) 
# of the images produced by the current or queried simulation run.
# 
# - make the file name pretty ;
# - build path strings from args and pretty file name ;
# - return http response ;
#
@only_safe
def show_plot(req, runref, name):
    # remove tags, eg. plot-Lmon1.sim-lin.gif => Lmon1.sim
    mon = name.split('-', 1)[1].rsplit('-', 1)[0]
    prefix = os.path.dirname(name)
    # construct paths
    doc_url = '/static/out/%s/%s/%s' % (runref, prefix, mon)
    img_url = '/static/out/%s/%s' % (runref, name)
    return HttpResponse('<a href="%s"><img src="%s"></a>' % (doc_url, img_url))


#  documentation()
# =================
# called from configure.html 'Simulation Selection' or 'Simulation Parameters' -> configure.js::update_defaults() -> urls.py
# 
# returns a http response but not 100% on what it points to.
# possibly the out/err docs, or, the uncompiled instrument file.
# Not exceptionally important right now, I am sure it will show itself sooner or later.
#
@only_safe
def documentation(req, instr):
    return HttpResponse(file('sim/%s.html' % instr).read(),
                        content_type='text/html')


#  latest()
# ==========
# called from configure.js:: switchLatest() -> configure.js::setLatest()
#
# - query McsimulatorSimRun DB for last 25 simulations ;
#   reverse order as pushed onto stack from the bottom.
# - return a dictionary of parameters from SimRun.queries ;
#
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

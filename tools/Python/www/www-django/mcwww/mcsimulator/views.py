#==============================================================#
# Edits: Mark Lewis 2015                                       #
# ----------------------                                       #
# Possible Job options:                                        #
# --------------------                                         #
#  Nice to have:                                               #
#   - actual home page with user saved results.                #
#     (when save works)                                        #
#   - drop box to choose different simulations.                #
#     (available in DB)                                        #
#                                                              #
#   - information and control of logged on accounts and        #
#     running simulations from admin accounts?                 #
#==============================================================#
# django imports
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User, Group
from django.shortcuts import redirect
from django.http import HttpResponse
# app imports
from mcsimulator.models import * # Will have to limit this import as going to build more DB objects that I don't want the std view to have access to.
from common import *
from mcwww.settings import STATIC_URL, MAX_RAY_SAMPLES, MAX_SCAN_POINTS
# consts
NONCE_NAME = 'csrfmiddlewaretoken'
#----------------------------------------------------------------------------#
# convertType()                                                              #
# -------------                                                              #
# called from configurePOST to populate simulation parameters and            #
# recursive, once, returns a list of typecast values                         #
#                                                                            #
# - test if the passed string is a ',' deliniated list                       #
# - RECURSIVE CALL : return list of values describved below                  #
# - return value contained in the string arg, typecast to default value type #
#----------------------------------------------------------------------------#
def convert_type(default, str_value):
    # tested types: str and float
    if ',' in str_value:
        return map(lambda elem: convert_type(default, elem), str_value.split(','))
    return type(default)(str_value)
#------------------------------------------------------------------------#
# home()                                                                 #
# ------                                                                 #
# - How is this method called?                                           #
# ASSUMING: dictionary supplies the correct value of                     #
# 'next' to login.html upon authenticated login                          #
#                                                                        #
# - get first Simulation Model from Simulation DB                        #
#   (populated w. default Simulations - ordering unknown)                #
# - create new Job Model (w. unique id etc.) containing default sim data #
# - save Job Model in Job DB                                             #
#   access jobs via job.id, known to user as randomised, o/p stored in   #
#   'out' on server                                                      #
# - redirect to simulation configuration page                            #
#------------------------------------------------------------------------#
@login_required
@only_safe
def home(req):
    sim = get_or_404(Simulation.objects.all()[:1])
    job = Job.new(ref=new_key(), sim=sim)
    job.save()
    return redirect('configure', jobref=job.ref)
#------------------------------------------------------------#
# configure()                                                #
# -----------                                                #
# redirected to by home()                                    #
# - get users Job from Job                                   #
#   job is either:                                           #
#         newly created by home()                            #
#         configure.html 'Simulation selection'              #
#                  -> configue.js::switchLatest()            #
#                             -> req argument                #
# - sims is filled with the users history of simulation runs #
#   this is used in configure.html to:                       #
#         populate 'List latest sinulations' drop menu       #
#         populate default param_set                         #
# - return the requested job id                              #
#------------------------------------------------------------#
@login_required
@only_safe
@templated('mcsimulator/configure') 
def configure(req, jobref): # This needs to be fixed so the user can access whatever simulation is available, relies on a job existing and being directed to.

    job = get_or_404(Job, ref=jobref)

    sims = Simulation.objects.all().filter(simgroup__in=req.user.groups.values_list('name',flat=True))
    return dict(job=job, jobid=job.ref, sims=sims, 
                max_samples=MAX_RAY_SAMPLES, max_npoints=MAX_SCAN_POINTS)
#--------------------------------------------------------------------------#
# configurePOST()                                                          #
# ---------------                                                          #
# configure.html form submission -> configure.js::run()                    #
#                                                                          #
# - read in the posted form from the request and request the simulation    #
#   from the McsimulatorSimulation DB                                      #
# - setup simulation and job using user defined params, checking with ok() #
# - save the job into the McsimulatorJob DB (jobref is passed as arg)      #
# - go through form, ignore pre-set params, store user defined params      #
# - return call to jsonify(list of errors tuple, list of oks)              #
#--------------------------------------------------------------------------#
@login_required
@only_unsafe
def configurePOST(req, jobref):
    oks    = []
    errors = []
    #------------------------------------------------#
    # ok()                                           #
    # ----                                           #
    # - if f() succeeds                              #
    #     add parameter name to oks list             #
    #     return f()'s return (set the parameter)    #
    # - if f() throws an exception (apparently could # 
    #   happen for all things not string or float)   #
    #   - print the error to the console             #
    #   - add parameter name to error list           #
    #   - return the old (maybe default) value       #
    #------------------------------------------------#
    def ok(name, old, f):
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
    job = get_or_404(Job, ref=jobref)
    # update job and set ok list
    job.seed    = ok("seed",    seed,    lambda : abs(int(form['seed'])))
    job.samples = ok("samples", samples, lambda : abs(int(form['samples'])))
    job.npoints = ok("samples", samples, lambda : abs(int(form['npoints'])))
    job.sim_id  = sim.id
    # insert / update params
    for name in skip((NONCE_NAME, 'sim', 'seed', 'samples', 'npoints'), form):
        str_value = form[name]
        # getting last used parameter from the param dictionary field
        old = job.params[name][1] or sim.params[name][1]           
        # typecast param value
        cast_val = ok(name, old, lambda : convert_type(param.default_value, str_value)) 
        job.params[name][1] = cast_val                    # ^---this will break.
    # save job
    job.save()
    return jsonify(errors=errors, oks=oks)
#-------------------------------------------------------------#
# simulatePOST()                                              #
# ----------------                                            #
# called by configure.html -> configure.js::run()             #
#                                                             #
# - get the Job model from McsimulatorJob DB                  #
# - set params with queries to McsimulatorJob model           #
# - filter:                                                   #
#   - valid = query McsimulatorJobs::McsimulatorSimulation DB #
#     to create set of parameter names                        #
#   - update the params dictionary (created above) using only #
#     those names that are stored in the valid set            #
# - build new SimRun model using user data and parameters and #
#   save to McSimulatorSimRun DB                              #
# - return a redirect to status page (which will call the     #
#   status method below)                                      #
#-------------------------------------------------------------#
@login_required
@only_unsafe
def simulatePOST(req, jobref):
    ''' Create simulation job for the worker '''
    job = get_or_404(Job, ref=jobref)
    # treat seed and samples specially
    params = { "_seed": job.seed,                                            # PARAMS
               "_samples": job.samples,
               "_npoints": job.npoints
               }
    # filter params by what the simulation expects (needed!)
    valid = set(name for name in job.sim.params.keys())
    params.update(dict(                                                      # PARAMS
            (name, val[1]) for pv in job.params.items()
            if name in valid))
    # create simulation run (for the worker to compute)
    run = SimRun.new(user=req.user, job=job, sim=job.sim, params=params)     # PARAMS
    run.save()
    # send user to status page
    return redirect('status', runref=run.ref)
#--------------------------------------------------------#
# status()                                               #
# --------                                               #
# Called by redirect to status page, simply provides the #
# dictionary with which the html/js can.                 #
# Request simulation o/p files from the server. Most     # 
# status update work done by js requests.                #
#                                                        #
# - query to McsimulatorSimRun DB for users current run  #
# - create parameter dictionary from run.queries         #
# - return entire dictionary of run/job/parameter data   #
#--------------------------------------------------------#
@only_safe
@templated('mcsimulator/status')
def status(req, runref, compN=None):
    run = get_or_404(SimRun, ref=runref)
    job = run.job
    params = sorted('%s=%s' % (p,v) for p,v in run.params.items()
                    if not p.startswith('_'))
    return dict(run=run, params=params,                             # PARAMS - THIS SHOULD WORK BUT IT MAY NOT.
                job=job, runref=runref, compN=compN)
#-----------------------------------------------------#
# showPlot()                                          #
# ----------                                          #
# takes requests from status.js and returns urls      #
#   (positions in the server dir structure)           #
#   of the images produced by the current or queried  #
#   simulation run.                                   #
#                                                     #
# - make the file name pretty                         #
# - build path strings from args and pretty file name #
# - return http response                              #
#-----------------------------------------------------#
@only_safe
def show_plot(req, runref, name):
    # remove tags, eg. plot-Lmon1.sim-lin.gif => Lmon1.sim
    mon = name.split('-', 1)[1].rsplit('-', 1)[0]
    prefix = os.path.dirname(name)
    # construct paths
    doc_url = '/static/out/%s/%s/%s' % (runref, prefix, mon)
    img_url = '/static/out/%s/%s' % (runref, name)
    return HttpResponse('<a href="%s"><img src="%s"></a>' % (doc_url, img_url))
#----------------------------------------------------------------#
# documentation()                                                #
# ---------------                                                #
# called from configure.html 'Simulation Selection', or,         #
#    'Simulation Parameters'                                     #
#       -> configure.js::update_defaults()                       #
#            -> urls.py                                          #
#                                                                #
# returns a http response but not 100% on what it points to.     #
# possibly the out/err docs, or, the uncompiled instrument file. #
# Not exceptionally important right now, I am sure it will show  #
# itself sooner or later.                                        #
#----------------------------------------------------------------#
@only_safe
def documentation(req, instr):
    return HttpResponse(file('sim/%s.html' % instr).read(),
                        content_type='text/html')
#---------------------------------------------------------#
# latest()                                                #
# --------                                                #
# called from configure.js::switchLatest()                #
#                -> configure.js::setLatest()             #
#                                                         #
# - query McsimulatorSimRun DB for last 25 simulations    #
#   reverse order as pushed onto stack from the bottom.   #
# - return a dictionary of parameters from SimRun.queries #
#---------------------------------------------------------#
# @app.route('/sim/latest', methods=['GET'])
@login_required
@only_safe
def latest(req):
    ''' List latest simulations as JSON '''
    runs = fetch(SimRun, 
                 user=req.user).order_by('-created').all()[:25]

    return jsonify(runs=[{'id'        : run.id,
                          'runref'    : run.ref,
                          'time'      : run.created.strftime('%s'),
                          'instrument': run.sim.name}
                         for run in runs])

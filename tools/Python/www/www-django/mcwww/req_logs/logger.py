def log_this(request, action, extra_strings):
    import datetime as dt
    day = dt.date.today()
    time = dt.datetime.now().strftime('%H:%M:%S')
    req_file = open("req_logs/%s-%s_%s.req"%(action,time,day), "w")
    req_file.write("%s" % request)
    a = 0
    for thing in extra_strings:
        req_file.write("%s: %s"%(a, thing))
    req_file.close

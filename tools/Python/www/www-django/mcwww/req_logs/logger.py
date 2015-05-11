def log_this(request, action):
    import datetime as dt
    print "this was called."
    day = dt.date.today()
    time = dt.datetime.now().strftime('%H:%M:%S')
    req_file = open("req_logs/%s-%s_%s.req"%(action,time,day), "w")
    req_file.write("%s" % request)
    req_file.close

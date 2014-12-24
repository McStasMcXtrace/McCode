def getForm():
    return dict(next = req.GET.get('next','/'))

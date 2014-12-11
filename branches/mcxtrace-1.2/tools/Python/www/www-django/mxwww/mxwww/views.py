from django.contrib.auth import authenticate, login, logout
from django.shortcuts import redirect

from common import templated


@templated('mxwww/login')
def login_form(req):
    return dict(next = req.GET.get('next', '/'))


def loginPOST(req):
    form = req.POST
    nexturl = form.get('next', '/')

    # grab user information
    usernm = form.get('username', '')
    passwd = form.get('password', '')

    # verify
    user = authenticate(username=usernm, password=passwd)
    if user is None or not user.is_active:
        return redirect('/login/?next=' + nexturl)

    # all ok, register user
    login(req, user)

    # continue to 'next' url
    return redirect(nexturl)



def logout_user(req):
    if req.user is not None:
        logout(req)
    return redirect('login_form')


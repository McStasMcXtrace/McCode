# authenticate was removed from contrib.auth in favour of LDAP auth.
from django.contrib.auth import login, logout 
from django.shortcuts import redirect
from sandbox.handy.common import templated

#  login_form()
# ==============
@templated('sandbox/templates/login')
def login_form(req):
    return dict(next = req.GET.get('next', '/'))

#  loginPOST()
# =============
# - take and process form data to get uid and pwd ;
# - use mcBackend authenticate tools to login mcUser(user) ;
# - not authenticated : send back to login.html ;
# - authenticated     : log user in ;
#                       send to <next>.html ;
def loginPOST(req):
    form = req.POST
    nexturl = form.get('next', '/')
    uid = form.get('uid', '')
    pwd = form.get('password', '')

    user = authenticate(uid=uid, password=pwd)
    if user is None or not user.is_active:
        return redirect('/login/?next=' + nexturl)
    login(req, user)
    return redirect(nexturl)

#  logout_user()
# ===============
# - use django authenticate tools to logout user ;
# - send to login.html ;
def logout_user(req):
    if req.user is not None:
        logout(req)
    return redirect('login_form')


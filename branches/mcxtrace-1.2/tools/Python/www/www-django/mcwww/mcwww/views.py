from django.contrib.auth import authenticate, login, logout
from django.shortcuts import redirect

from common import templated

# Edited : Mark Lewis 2014
# Added method descriptors formatted:

#  methodName()
# ==============
# called from
#
# - algorithm whatsits
# Blah bloops
# - more algy nonsence
#

'''inline comments added like this '''
'''--------------------------------------------------------'''



#  login_form()
# ==============
# not 100% on how the dictionary knows what to return but it does.
#

@templated('mcwww/login')
def login_form(req):
    return dict(next = req.GET.get('next', '/'))

#  loginPOST()
# =============
# called from login.html -> urls.py 
#
# - use django authenticate tools to login user ;
# - process the data in the form to authenticate a user ;
# - not authenticaed users set back to login page ;
# - log authenticated users in and send to configure.html ;
#

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

#  logout_user()
# ===============
# called from configure.html|status.html -> urls.py
#
# - use django authenticate tools to logout user ;
# - send to login.html ;
#

def logout_user(req):
    if req.user is not None:
        logout(req)
    return redirect('login_form')


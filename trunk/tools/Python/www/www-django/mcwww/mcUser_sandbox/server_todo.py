
#=========================#
# User manipulation views #
#=========================#
@builder.assign_pattern(r'^userSetup/$')
def UserSetup(req):
    return render(req, 'mcUserUserSetup.html', {})
@builder.assign_pattern(r'^adminSetup/$')
def AdminSetup(req):
    return render(req, 'mcUserAdminSetup.html', {})


# NEED TO BUILD #
#===============#
# - userChangePOST
# - userView
# - adminView
# - other views, I think.


# Start up script : allows to call from command line - runs same commands as ./manage.py
if __name__=='__main__':
    from django.core.management import execute_from_command_line
    execute_from_command_line()

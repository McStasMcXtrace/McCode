# Template for LDAP access data container
#
# attributes:  file location, dn, pw, cn, sn, mail, uid
# create uid based on DB query for name
#  
# This will be extended to make use of all the neccessary attributes

class LDAPData:
# Returning the information
    def __init__(self, un):
# Make setter methods for these.
        self.ldif_file = None
        self.CN = None
        self.SN = None
        self.displayName = None
        self.MAIL = None
        self.PASSWORD = None
# std uid is 4 or 5 letters long
        self.UID = self.setuid(un)

# Set methods for relevent info
    def setldif_file(self, filepath):
        self.ldif_file = filepath

    def setnames(self, FN, SN):
        self.SN = SN
        self.CN = FN + SN
        self.displayName = FN 

    def setmail(self, M):
        self.MAIL = M

    def setuid(self, un):
        if(len(un) > 5): self.UID = un[0:4]
        else: self.UID = un
        
    def setpassword(self, pw):
        self.PASSWORD = pw
        

# return methods for class attributes
    def ldif(self):
        return self.ldif_file
    def cn(self):
        return self.CN
    def sn(self):
        return self.SN
    def uid(self):
        return self.UID
    def mail(self):
        return self.MAIL
    def displayname(self):
        return self.displayName
    def password(self):
        return self.PASSWORD

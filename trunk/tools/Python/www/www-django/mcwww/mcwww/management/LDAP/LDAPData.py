# Template for LDAP access data container
#
# attributes:  file location, dn, pw, cn, sn, mail, uid
# create uid based on DB query for name
#  
# If this was c++ i'd use a struct here. If anyone has an idea of 
# how to implement something similar I would appreciate.
# I like python but I miss c++ a *lot*
#
# This will be extended to make use of all the neccessary attributes

class LDAPData:
# Returning the information
    def __init__(self): #:, un):
# Contant Value Attrs
        self.CN = None
        self.SN = None
        self.displayName = None
        self.MAIL = None
# Temporary Value Attrs - always blank after use
        self.ldif_file = None
        self.PASSWORD = None
# std uid is 4 or 5 letters long
#       self.UID = self.setuid(un)

# Constant Variable setters/Getters
    def setnames(self, FN, SN):
        if !self.displayName:
            self.SN = SN
            self.CN = FN + SN
            self.displayName = FN 
    def cn(self):
        return self.CN
    def sn(self):
        return self.SN
    def setdisplayname(self, name):
        self.displayname = name
    def displayname(self):
        return self.displayName
    def setmail(self, M):
        if !MAIL:
            self.MAIL = M
    def mail(self):
        return self.MAIL
# IMPORTANT - MUST EDIT #
#=======================#
# QUERY TO SEE IF SIMILAR UID DB #
# Have to set up LDAP to allow anyone to read uids #
    def setuid(self, un):
        if !UID:
            if(len(un) > 5): self.UID = un[0:4]
            else: self.UID = un
    def uid(self):
        return self.UID

# Temporary Value Setter/Getters
    def setldif_file(self, filepath):
        self.ldif_file = filepath        
    def ldif(self):
        return self.ldif_file
    def setpassword(self, pw):
        self.PASSWORD = pw
    def password(self):
        return self.PASSWORD


#
# LDAPData object builder
# -----------------------
# - This should be imported, not the LDApData object, if we can help it
#   we can return the LDAPData object from here once it has been built.
#

class LDAPDataBuilder:
    def __init__(self, full_name, mail, pwd):
        self.data = LDAPDData(full_name)
        uid = None
        splitname = split(" ", full_name)
        splitlen = len(splitname)

        self.data.setnames(splitname[0], splitname[splitlen-1])
        if splitlen = 2:
            uid = splitname[0][0]+splitname[1][0,3]
        else: 
            s = 0
            for name in splitname:
                uid += name[0]
                s+=1
            while len(uid) <= 5: 
                uid += splitname[splitlen][s+=1]
        self.data.setuid(uid)
        self.data.setmail(mail)
    
    def getLDIF(self):
        

    def getData(self):
        return self.data

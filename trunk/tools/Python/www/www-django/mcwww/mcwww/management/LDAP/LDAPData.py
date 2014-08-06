#=====================#
# LDAP data container #
#=====================#
class LDAPData:
    def __init__(self):
        self.UID = None
        self.CN = None
        self.SN = None
        self.displayName = None
        self.MAIL = None

        self.ldif_file = None
        self.PASSWORD = None

    def setcn(self, cn):
        self.CN = cn
    def cn(self):
        return self.CN

    def setsn(self, sn):
        self.SN = sn
    def sn(self):
        return self.SN

    def setdisplayname(self, name):
        self.displayname = name
    def displayname(self):
        return self.displayName

    def setmail(self, M):
        self.MAIL = M
    def mail(self):
        return self.MAIL

    def setuid(self, un):
        self.UID = un
    def uid(self):
        return self.UID

    def setldif_file(self, filepath):
        self.ldif_file = filepath        
    def ldif(self):
        return self.ldif_file

    def setpassword(self, pw):
        self.PASSWORD = pw
    def password(self):
        return self.PASSWORD


#=========================#
# LDAPData object builder # used by createmcuser
#=========================#
class LDAPDataBuilder:
    def __init__(self, full_name, mail):
        self.data = LDAPDData()
        uid = None
        splitname = split(" ", full_name)
        splitlen = len(splitname)-1
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
        self.data.setcn("%s %s" % splitname[0], splitname[splitlen])
        self.data.setsn(splitname[splitlen])
        self.data.setdisplayname(splitname[0])
        self.data.setmail(mail)
        
    def getData(self):
        return self.data

#===========================#
# LDAPData object populator # used by authenticate to drag data from the LDAP DB
#===========================#
class LDAPDataPopulator:
    def __init__(self, cn, pw):
        data = LDAPData()
        query = "cn=\"%s\"" % cn
        dn = "dn=\"cn=%s,ou=person,dc=fysik,dc=dtu,dc=dk\"" % cn
        usr_details = conn.ldapQuery(dn, pw, query)
        for line in usr_details:
            if 'uid' in line:
                data.setuid(split(" ", line)[1])
            if 'cn' in line:
                data.setcn(split(" ", line)[1])
            if 'sn' in line:
                data.setsn(split(" ", line)[1])
            if 'displayname' in line:
                data.setdisplayname(split(" ", line)[1])
            if 'email' in line:
                data.setmail(split()" ", line)[1])

    def getData(self):
        return self.data

def LDAPDict(key):
    return {
        
    

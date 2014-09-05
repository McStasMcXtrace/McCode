#=====================#
# LDAP data container #
#=====================#
class LDAPData:
    def __init__(self):
        self.UID = None
        self.CN = None
        self.SN = None
        self.MAIL = None
        self.displayName = None

        self.ldif_file = None
        self.PASSWORD = None

    def setcn(self, cn):
        if not self.CN: self.CN = cn
    def cn(self):
        return self.CN

    def setsn(self, sn):
        if not self.SN: self.SN = sn
    def sn(self):
        return self.SN


    def setmail(self, M):
        if not self.MAIL: self.MAIL = M
    def mail(self):
        return self.MAIL

    def setuid(self, un):
        if not self.UID: self.UID = un
    def uid(self):
        return self.UID

    def setdisplayname(self, name):
        self.displayname = name
    def displayname(self):
        return self.displayName

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
        if splitlen == 2:
            uid = splitname[0][0]+splitname[1][0,3]
        else: 
            s = 0
            for name in splitname:
                uid += name[0]
                s+=1
            while len(uid) <= 5: 
                uid += splitname[splitlen][s]
                s+=1
        self.data.setuid(uid)
        self.data.setcn("%s %s" % splitname[0], splitname[splitlen])
        self.data.setsn(splitname[splitlen])
        self.data.setmail(mail)
        self.data.setdisplayname(splitname[0])
        
    def getData(self):
        return self.data

#===========================#
# LDAPData object populator # used by authenticate to drag data from the LDAP DB
#===========================#
class LDAPDataPopulator:
    def __init__(self, cn, pw):
        self.data = LDAPData()
        conn = LDAPComm()
        query = "cn=\"%s\"" % cn
        dn = "dn=\"cn=%s,ou=person,dc=fysik,dc=dtu,dc=dk\"" % cn
        usr_details = conn.ldapQuery(dn, pw, query)
        for line in usr_details:
            if 'uid' in line:
                self.data.setuid(split(" ", line)[1])
            if 'cn' in line:
                self.data.setcn(split(" ", line)[1])
            if 'sn' in line:
                self.data.setsn(split(" ", line)[1])
            if 'email' in line:
                self.data.setmail(split(" ", line)[1])
            if 'displayname' in line:
                self.data.setdisplayname(split(" ", line)[1])

    def getData(self):
        return self.data        

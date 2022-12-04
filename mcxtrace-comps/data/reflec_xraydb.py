import numpy as np
from xraydb import mirror_reflectivity
import sys

#python3 reflec_xraydb.py E_min(eV) E_max theta_min(deg) theta_max AtomName density(g/cm^3)
#examples: 
#python3 reflec_xraydb.py 5600 46300 0.0001 2 Pd 12.023
#python3 reflec_xraydb.py 5600 46300 0.0001 2 Pt 21.45
#python3 reflec_xraydb.py 5600 46300 0.0001 2 B4C 2.52

#python3 reflec_xraydb.py 3400 29000 0.0001 2 Pd 12.023
#python3 reflec_xraydb.py 3400 29000 0.0001 2 Pt 21.45
#python3 reflec_xraydb.py 3400 29000 0.0001 2 B4C 2.52

#python3 reflec_xraydb.py 3400 19000 0.0001 2 Pd 12.023
#python3 reflec_xraydb.py 3400 19000 0.0001 2 Pt 21.45
#python3 reflec_xraydb.py 3400 19000 0.0001 2 B4C 2.52
n = len(sys.argv)
print("Total arguments passed:", n)
if n!=7:
    print("6 args needed: E_min(eV) E_max theta_min(deg) theta_max AtomName density(g/cm^3)")
else:
    E_min=float(sys.argv[1])
    E_max=float(sys.argv[2])
    E_nb=200
    E_step=(E_max-E_min)/(E_nb-1)
    liste_e = []
    liste_e.append(E_min)

    for i in range(1,E_nb):
        liste_e.append(E_min+i*E_step)    
        
    print("E_max-E_min",E_max-E_min)
    print("E_nb-1",E_nb-1,"E_step",E_step)
    print("(E_nb-1)*E_step",(E_nb-1)*E_step)
        
    #in radians
    theta_min=float(sys.argv[3])*np.pi/180
    theta_max=float(sys.argv[4])*np.pi/180
    theta_nb=2000
    theta_step=(theta_max-theta_min)/(theta_nb-1)
    theta = np.linspace(theta_min, theta_max, theta_nb)

    print("theta_max-theta_min",(theta_max-theta_min)*180/np.pi)
    print("theta_nb-1",theta_nb-1,"theta_step",theta_step*180/np.pi)
    print("(theta_nb-1)*theta_step",(theta_nb-1)*theta_step*180/np.pi)

    filename = sys.argv[5]+".dat"
    print(theta/np.pi*180)
    print(liste_e)
    #mirror_reflectivity, other parameters: density, roughness, polarization ... 
    #see https://xraypy.github.io/XrayDB/python.html#xraydb.mirror_reflectivity
    with open(filename,'w') as f:
        #for the file: E in kev, theta in deg
        f.write("#param=eth\n")
        f.write("#E_min="+str(E_min*10**(-3))+"\n")
        f.write("#E_max="+str(E_max*10**(-3))+"\n")
        f.write("#E_step="+str(E_step*10**(-3))+"\n")
        f.write("#theta_min="+str(theta_min*180/np.pi)+"\n")
        f.write("#theta_max="+str(theta_max*180/np.pi)+"\n")
        f.write("#theta_step="+str(theta_step*180/np.pi)+"\n")
        for single_energy in liste_e:
            #print(single_energy)
            r_si = mirror_reflectivity(sys.argv[5], theta, single_energy, density=float(sys.argv[6]))
            for single_ref in r_si:            
                f.write(str(single_ref)+"      ")
            f.write('\n')






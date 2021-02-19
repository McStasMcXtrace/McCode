#!/usr/bin/env python
import glob
import pathlib
import subprocess
import tempfile
import fire

class McTestcomp:
  def __init__(self,flavour='mcstas',compiler='gcc',cflags='-lm'):
    #self.comppath=pathlib.Path(inpath)
    #self.compname=self.path_to_compname(comppath)
    self.set_flavour(flavour)
    self.compiler=compiler
    self.cflags=cflags

  def path_to_compname(self,path):
    #path is a ssumed to be a path object
    return path.stem

  def set_flavour(self,flavour="mcstas"):
     self.flavour=flavour

  def set_path_tree(path=None,pathstring=None):
    if ( path.exists() and path.isdir()):
      #assume a path-oject is passed
      pass
    else:
      pass

  def code_cenerate(self):
    pass

  def run(self, *comps):
    self.results=[]
    print(comps)
    for compname in comps:
      #write the test instrument to a temporary file
      with tempfile.NamedTemporaryFile(delete=False) as tf:
        print(tf.name)
        tf.write(self.test_instrument(compname).encode())
        tf.seek(0)
        tf.close()
        compile_return=False
        cogen_return=self.run_cogen(tf)
        if(cogen_return):
          compile_return=self.run_compiler(tf)
        #need to cleanup a bit
        #return (cogen_return.returncode,compile_return.returncode)
      self.results.append({'name':compname,'cogen_res':cogen_return.returncode,'compile_res':compile_return.returncode})
      print(self.results)
    return self

  def run_cogen(self,instr):
    print(" ".join([f"{self.flavour}",f"{instr.name}","-o",f"{instr.name}.c"]))
    return subprocess.run([f"{self.flavour}","-t",f"{instr.name}","-o",f"{instr.name}.c"])

  def run_compiler(self,instr):
    #this needs to somehow deal with DEPENDENCY also, through m[cx]run?
    args=[f"{self.compiler}",f"{instr.name}.c","-o",f"{instr.name}.out",self.cflags]
    args.append(self.cflags)
    return subprocess.run(args)
    #[f"{self.compiler}",f"{instr.name}.c","-o",f"{instr.name}.out"])
  
  def test_instrument(self,compname,args=""):
    return f"""DEFINE INSTRUMENT Test{compname}(dummy=1)

INITIALIZE
%{{
%}}

TRACE

COMPONENT origin = Arm()
AT (0, 0, 0) RELATIVE ABSOLUTE

COMPONENT comp = {compname}({args})
AT(0,0,0) RELATIVE origin

END
"""

  def __str__(self):
    print("component  cogen?   compile?")
    return "\n".join( "%12s  %12s %12s" % (r['name'],r['cogen_res'],r['compile_res']) for r in self.results)


  def print_instrument(self):
    print(self.test_instrument())

  def mcrun_instrument():
    pass

if __name__=='__main__':
  out=fire.Fire(McTestcomp) 

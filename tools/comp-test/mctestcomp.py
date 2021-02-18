#!/usr/bin/env python
import glob
import pathlib
import subprocess
import tempfile

class McTestcomp:
  def __init__(self,comppath):
    self.comppath=comppath
    self.compname=self.path_to_compname(comppath)
    self.set_flavour('mcstas')
    self.compiler='gcc'
    self.cflags='-lm'

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

  def run(self):
    #write the test instrument to a temporary file
    with tempfile.NamedTemporaryFile(delete=False) as tf:
      print(tf.name)
      tf.write(self.test_instrument().encode())
      tf.seek(0)
      tf.close()
      cogen_return=self.run_cogen(tf)
      print(cogen_return)
      compile_return=self.run_compiler(tf)
      #need to cleanup a bit
      return (cogen_return.returncode,compile_return.returncode)

  def run_cogen(self,instr):
    print(" ".join([f"{self.flavour}","-t",f"{instr.name}","-o",f"{instr.name}.c"]))
    return subprocess.run([f"{self.flavour}","-t",f"{instr.name}","-o",f"{instr.name}.c"])

  def run_compiler(self,instr):
    #this needs to somehow deal with DEPENDENCY also, through m[cx]run?
    args=[f"{self.compiler}",f"{instr.name}.c","-o",f"{instr.name}.out",self.cflags]
    args.append(self.cflags)
    return subprocess.run(args)
    #[f"{self.compiler}",f"{instr.name}.c","-o",f"{instr.name}.out"])

  def test_instrument(self):
    return f"""DEFINE INSTRUMENT Test{self.compname}(dummy=1)

INITIALIZE
%{{
%}}

TRACE

COMPONENT origin = Arm()
AT (0, 0, 0) RELATIVE ABSOLUTE

COMPONENT comp = {self.compname}()
AT(0,0,0) RELATIVE origin

END
"""

  def print_instrument(self):
    print(self.test_instrument())

  def mcrun_instrument():
    pass

if __name__=='__main__':
  results={}

  p=pathlib.Path('Source_simple.comp')
  t=McTestcomp(p)
  rcs=t.run()
  results.update({p.name:rcs})

  print(results)

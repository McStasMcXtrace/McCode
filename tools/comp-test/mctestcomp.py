#!/usr/bin/env python
import glob
import pathlib
import subprocess
import tempfile
import fire
import os

class McTestcomp:
  def __init__(self,flavour='mcstas',compiler='gcc',cflags='-lm', root='default'):
    #self.comppath=pathlib.Path(inpath)
    #self.compname=self._path_to_compname(comppath)
    self.root=root
    self._set_flavour(flavour)
    self.compiler=compiler
    self.cflags=cflags
    self.results=[]

  def _path_to_compname(self,path):
    #path is a ssumed to be a path object
    return path.stem

  def _set_flavour(self,flavour="mcstas"):
    self.flavour=flavour
    print(self.root)
    if self.root!=None:
      d=os.environ.copy()
      d.update({self.flavour.upper() : self.root})
      self.env=d

  def _set_path_tree(path=None,pathstring=None):
    if ( path.exists() and path.isdir()):
      #assume a path-oject is passed
      pass
    else:
      pass

  def run(self, *comps):
    print(comps)
    for compname in comps:
      #write the test instrument to a temporary file
      with tempfile.NamedTemporaryFile(delete=False,dir='.') as tf:
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
    return subprocess.run([f"{self.flavour}","-t",f"{instr.name}","-o",f"{instr.name}.c"],env=self.env)

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

COMPONENT comp = {compname}({args})
AT(0,0,0) RELATIVE ABSOLUTE

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

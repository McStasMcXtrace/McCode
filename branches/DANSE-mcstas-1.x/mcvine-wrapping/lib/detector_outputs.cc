// -*- C++ -*-
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//                                   Jiao Lin
//                      California Institute of Technology
//                        (C) 2005 All Rights Reserved
//
// {LicenseText}
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//


#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <ctype.h>
#include <cstring>
#include "detector_outputs.h"
#include "misc_macros.h"


//stripped off from mcstas-r.cc



//-------------- global vars ---------------
//to use global variable usually causes problem

//mcdirname was a global variable to indicate path to working directory of current
//simulation
//here let us not use this variable
static char * mcdirname = 0;

//mcstartdate records the time the simulation starts
const static long mcstartdate = 0;

//mcinstrument_name
static char * mcinstrument_name = "InstrumentName";

//mcinstrument_sorce
static char * mcinstrument_source = "InstrumentSource";

//number of input parameter
const static int mcnumipar = 0;


// these definitions only help define "mcinputtable"
/* Note: the enum instr_formal_types definition MUST be kept
   synchronized with the one in mcstas.h and with the
   instr_formal_type_names array in cogen.c. */
enum instr_formal_types
  {
    instr_type_double, instr_type_int, instr_type_string
  };
struct mcinputtable_struct {
  char *name;
  void *par;
  enum instr_formal_types type;
  char *val;
};
//table of input parameters
struct mcinputtable_struct mcinputtable[1] = {
  NULL, NULL, instr_type_double, ""
};


//
const static bool mctraceenabled = 1;

//
const static bool mcdefaultmain = 1;

//
const static bool mcdotrace = 1;

//
const static bool mcgravitation = 1;

//
const static long mcseed = 0;

//
const static int mcdisable_output_files = 0;

//
static char *mcsiminfo_name= "mcstas";

//
static bool mcsingle_file = 0;

//
bool mcascii_only = 0;

//
FILE *mcsiminfo_file=NULL;


#ifdef WIN32
#define MC_PATHSEP_C '\\'
#define MC_PATHSEP_S "\\"
#else  /* !WIN32 */
#ifdef MAC
#define MC_PATHSEP_C ':'
#define MC_PATHSEP_S ":"
#else  /* !MAC */
#define MC_PATHSEP_C '/'
#define MC_PATHSEP_S "/"
#endif /* !MAC */
#endif /* !WIN32 */




// format strings
#define mcNUMFORMATS 8
#ifndef MCSTAS_FORMAT
#define MCSTAS_FORMAT "McStas"  /* default format */
#endif
struct mcformats_struct mcformat;

static struct mcformats_struct mcformats[mcNUMFORMATS] = {
  { "McStas", "sim",
    "%1$sFormat: %4$s file\n"
      "%1$sURL:    http://neutron.risoe.dk/\n"
      "%1$sEditor: %6$s\n"
      "%1$sCreator:%2$s simulation (McStas " MCSTAS_VERSION ")\n"
      "%1$sDate:   Simulation started (%8$li) %5$s\n"
      "%1$sFile:   %3$s\n",
    "%1$sEndDate:%5$s\n",
    "%1$sbegin %2$s\n",
    "%1$send %2$s\n",
    "%1$s%3$s: %4$s\n",
    "", 
    "%1$sErrors [%2$s/%4$s]: \n",
    "%1$sEvents [%2$s/%4$s]: \n",
    "", "", "" },
  { "Scilab", "sci",
    "function mc_%7$s = get_%7$s(p)\n"
      "// %4$s function issued from McStas on %5$s\n"
      "// McStas simulation %2$s: %3$s" MC_PATHSEP_S "%4$s\n"
      "// import data using exec('%7$s.sci',-1); s=get_%7$s('plot');\nmode(-1); //silent execution\n"
      "if argn(2) > 0, p=1; else p=0; end\n"
      "mc_%7$s = struct();\n"
      "mc_%7$s.Format ='%4$s';\n"
      "mc_%7$s.URL    ='http://neutron.risoe.dk';\n"
      "mc_%7$s.Editor ='%6$s';\n"
      "mc_%7$s.Creator='%2$s McStas " MCSTAS_VERSION " simulation';\n"
      "mc_%7$s.Date   =%8$li; // for getdate\n"
      "mc_%7$s.File   ='%3$s';\n",
    "mc_%7$s.EndDate=%8$li; // for getdate\nendfunction\n"
    "function d=mcload_inline(d)\n"
      "// local inline func to load data\n"
      "execstr(['S=['+part(d.type,10:(length(d.type)-1))+'];']);\n"
      "if ~length(d.data)\n"
      " if ~length(strindex(d.format, 'binary'))\n"
      "  exec(d.filename,-1);p=d.parent;\n"
      "  if ~execstr('d2='+d.func+'();','errcatch'),d=d2; d.parent=p;end\n"
      " else\n"
      "  if length(strindex(d.format, 'float')), t='f';\n"
      "  elseif length(strindex(d.format, 'double')), t='d';\n"
      "  else return; end\n"
      "  fid=mopen(d.filename, 'rb');\n"
      "  pS = prod(S);\n"
      "  x = mget(3*pS, t, fid);\n"
      "  d.data  =matrix(x(1:pS), S);\n"
      "  if length(x) >= 3*pS,\n"
      "  d.errors=matrix(x((pS+1):(2*pS)), S);\n"
      "  d.events=matrix(x((2*pS+1):(3*pS)), S);end\n"
      "  mclose(fid);\n"
      "  return\n"
      " end\n"
      "end\n"
      "endfunction\n"
      "function d=mcplot_inline(d,p)\n"
      "// local inline func to plot data\n"
      "if ~length(strindex(d.type,'0d')), d=mcload_inline(d); end\n"
      "if ~p, return; end;\n"
      "execstr(['l=[',d.xylimits,'];']); S=size(d.data);\n"
      "t1=['['+d.parent+'] '+d.filename+': '+d.title];t = [t1;['  '+d.variables+'=['+d.values+']'];['  '+d.signal];['  '+d.statistics]];\n"
      "mprintf('%%s\\n',t(:));\n"
      "if length(strindex(d.type,'0d')),return;\n"
      "else\nw=winsid();if length(w),w=w($)+1; else w=0; end\n"
      "xbasr(w); xset('window',w);\n"
      "if length(strindex(d.type,'2d'))\n"
      " d.x=linspace(l(1),l(2),S(2)); d.y=linspace(l(3),l(4),S(1)); z=d.data;\n"
      " xlab=d.xlabel; ylab=d.ylabel; x=d.x; y=d.y;\n"
      " fz=max(abs(z));fx=max(abs(d.x));fy=max(abs(d.y));\n"
      " if fx>0,fx=round(log10(fx)); x=x/10^fx; xlab=xlab+' [*10^'+string(fx)+']'; end\n"
      " if fy>0,fy=round(log10(fy)); y=y/10^fy; ylab=ylab+' [*10^'+string(fy)+']'; end\n"
      " if fz>0,fz=round(log10(fz)); z=z/10^fz; t1=t1+' [*10^'+string(fz)+']'; end\n"
      " xset('colormap',hotcolormap(64));\n"
      " plot3d1(x,y,z',90,0,xlab+'@'+ylab+'@'+d.zlabel); xtitle(t);\n"
      "else\nd.x=linspace(l(1),l(2),max(S));\n"
      " plot2d(d.x,d.data);xtitle(t,d.xlabel,d.ylabel);end\nend\n"
      "xname(t1);\nendfunction\n"
    "mc_%7$s=get_%7$s();\n",
    "// Section %2$s [%3$s] (level %7$d)\n"
      "%1$st=[]; execstr('t=mc_%4$s.class','errcatch'); if ~length(t), mc_%4$s = struct(); end; mc_%4$s.class = '%2$s';",
    "%1$smc_%6$s.mc_%4$s = 0; mc_%6$s.mc_%4$s = mc_%4$s;\n",
    "%1$smc_%2$s.%3$s = '%4$s';\n",
    "%1$smc_%2$s.func='get_%2$s';\n%1$smc_%2$s.data = [ ",
    "%1$serrors = [ ",
    "%1$sevents = [ ",
    " ]; // end of data\n%1$sif length(mc_%2$s.data) == 0, single_file=0; else single_file=1; end\n%1$smc_%2$s=mcplot_inline(mc_%2$s,p);\n",
    " ]; // end of errors\n%1$sif single_file == 1, mc_%2$s.errors=errors; end\n",
    " ]; // end of events\n%1$sif single_file == 1, mc_%2$s.events=events; end\n"},
  { "Matlab", "m",
    "function mc_%7$s = get_%7$s(p)\n"
      "%% %4$s function issued from McStas on %5$s\n"
      "%% McStas simulation %2$s: %3$s\n"
      "%% import data using s=%7$s('plot');\n"
      "if nargout == 0 | nargin > 0, p=1; else p=0; end\n"
      "mc_%7$s.Format ='%4$s';\n"
      "mc_%7$s.URL    ='http://neutron.risoe.dk';\n"
      "mc_%7$s.Editor ='%6$s';\n"
      "mc_%7$s.Creator='%2$s McStas " MCSTAS_VERSION " simulation';\n"
      "mc_%7$s.Date   =%8$li; %% for datestr\n"
      "mc_%7$s.File   ='%3$s';\n",
    "mc_%7$s.EndDate=%8$li; %% for datestr\n"
      "function d=mcload_inline(d)\n"
      "%% local inline function to load data\n"
      "S=d.type; eval(['S=[ ' S(10:(length(S)-1)) ' ];']);\n"
      "if isempty(d.data)\n"
      " if ~length(findstr(d.format, 'binary'))\n"
      "  copyfile(d.filename,[d.func,'.m']);p=d.parent;path(path);\n"
      "  eval(['d=',d.func,';']);d.parent=p;delete([d.func,'.m']);\n"
      " else\n"
      "  if length(findstr(d.format, 'float')), t='single';\n"
      "  elseif length(findstr(d.format, 'double')), t='double';\n"
      "  else return; end\n"
      "  if length(S) == 1, S=[S 1]; end\n"
      "  fid=fopen(d.filename, 'r');\n"
      "  pS = prod(S);\n"
      "  x = fread(fid, 3*pS, t);\n"
      "  d.data  =reshape(x(1:pS), S);\n"
      "  if prod(size(x)) >= 3*pS,\n"
      "  d.errors=reshape(x((pS+1):(2*pS)), S);\n"
      "  d.events=reshape(x((2*pS+1):(3*pS)), S);end\n"
      "  fclose(fid);\n"
      "  return\n"
      " end\n"
      "end\n"
      "return;\n"
      "function d=mcplot_inline(d,p)\n"
      "%% local inline function to plot data\n"
      "if isempty(findstr(d.type,'0d')), d=mcload_inline(d); end\nif ~p, return; end;\n"
      "eval(['l=[',d.xylimits,'];']); S=size(d.data);\n"
      "t1=['[',d.parent,'] ',d.filename,': ',d.title];t = strvcat(t1,['  ',d.variables,'=[',d.values,']'],['  ',d.signal],['  ',d.statistics]);\n"
      "disp(t);\n"
      "if ~isempty(findstr(d.type,'0d')), return; end\n"
      "figure; if ~isempty(findstr(d.type,'2d'))\n"
      "d.x=linspace(l(1),l(2),S(2)); d.y=linspace(l(3),l(4),S(1));\n"
      "surface(d.x,d.y,d.data);\n"
      "else\nd.x=linspace(l(1),l(2),max(S));\nplot(d.x,d.data);end\n"
      "xlabel(d.xlabel); ylabel(d.ylabel); title(t); axis tight;"
      "set(gca,'position',[.18,.18,.7,.65]); set(gcf,'name',t1);grid on;\n"
      "if ~isempty(findstr(d.type,'2d')), colorbar; end\n",
    "%% Section %2$s [%3$s] (level %7$d)\n"
      "mc_%4$s.class = '%2$s';",
    "mc_%6$s.mc_%4$s = mc_%4$s;\n",
    "%1$smc_%2$s.%3$s = '%4$s';\n",
    "%1$smc_%2$s.func='%2$s';\n%1$smc_%2$s.data = [ ",
    "%1$serrors = [ ",
    "%1$sevents = [ ",
    " ]; %% end of data\nif length(mc_%2$s.data) == 0, single_file=0; else single_file=1; end\nmc_%2$s=mcplot_inline(mc_%2$s,p);\n",
    " ]; %% end of errors\nif single_file, mc_%2$s.errors=errors; end\n",
    " ]; %% end of events\nif single_file, mc_%2$s.events=events; end\n"},
  { "IDL", "pro",
    "function mcload_inline,d\n"
      "; local inline function to load external data\n"
      "S=d.type & a=execute('S=long(['+strmid(S,9,strlen(S)-10)+'])')\n"
      "if strpos(d.format, 'binary') lt 0 then begin\n"
      " p=d.parent\n"
      " x=read_binary(d.filename)\n"
      " get_lun, lun\n"
      " openw,lun,d.func+'.pro'\n"
      " writeu, lun,x\n"
      " free_lun,lun\n"
      " resolve_routine, d.func, /is_func, /no\n"
      " d=call_function(d.func)\n"
      "endif else begin\n"
      " if strpos(d.format, 'float') ge 0 then t=4 $\n"
      " else if strpos(d.format, 'double') ge 0 then t=5 $\n"
      " else return,d\n"
      " x=read_binary(d.filename, data_type=t)\n"
      " pS=n_elements(S)\nif pS eq 1 then pS=long(S) $\n"
      " else if pS eq 2 then pS=long(S(0)*S(1)) $\n"
      " else pS=long(S(0)*S(1)*S(2))\n"
      " pS=pS(0)\nstv,d,'data',reform(x(0:(pS-1)),S)\n"
      " d.data=transpose(d.data)\n"
      " if n_elements(x) ge long(3*pS) then begin\n"
      "  stv,d,'errors',reform(x(pS:(2*pS-1)),S)\n"
      "  stv,d,'events',reform(x((2*pS):(3*pS-1)),S)\n"
      "  d.errors=transpose(d.errors)\n"
      "  d.events=transpose(d.events)\n"
      " endif\n"
      "endelse\n"
      "return,d\nend ; FUN load\n"
    "function mcplot_inline,d,p\n"
      "; local inline function to plot data\n"
      "if size(d.data,/typ) eq 7 and strpos(d.type,'0d') lt 0 then d=mcload_inline(d)\n"
      "if p eq 0 or strpos(d.type,'0d') ge 0 then return, d\n"
      "S=d.type & a=execute('S=long(['+strmid(S,9,strlen(S)-10)+'])')\n"
      "stv,d,'data',reform(d.data,S,/over)\n"
      "if total(strpos(tag_names(d),'ERRORS')+1) gt 0 then begin\n"
      " stv,d,'errors',reform(d.errors,S,/over)\n"
      " stv,d,'events',reform(d.events,S,/over)\n"
      "endif\n"
      "d.xylimits=strjoin(strsplit(d.xylimits,' ',/extract),',') & a=execute('l=['+d.xylimits+']')\n"
      "t1='['+d.parent+'] '+d.filename+': '+d.title\n"
      "t=[t1,'  '+d.variables+'=['+d.values+']','  '+d.signal,'  '+d.statistics]\n"
      "print,t\n"
      "if strpos(d.type,'0d') ge 0 then return,d\n"
      "d.xlabel=strjoin(strsplit(d.xlabel,'`!\"??&*()-+=|\\,.<>/?@''~#{[}]',/extract),'_')\n"
      "d.ylabel=strjoin(strsplit(d.ylabel,'`!\"??&*()-+=|\\,.<>/?@''~#{[}]',/extract),'_')\n"
      "stv,d,'x',l(0)+indgen(S(0))*(l(1)-l(0))/S(0)\n"
      "if strpos(d.type,'2d') ge 0 then begin\n"
      "  name={DATA:d.func,IX:d.xlabel,IY:d.ylabel}\n"
      "  stv,d,'y',l(2)+indgen(S(1))*(l(3)-l(2))/S(1)\n"
      "  live_surface,d.data,xindependent=d.x,yindependent=d.y,name=name,reference_out=Win\n"
      "endif else begin\n"
      "  name={DATA:d.func,I:d.xlabel}\n"
      "  live_plot,d.data,independent=d.x,name=name,reference_out=Win\n"
      "endelse\n"
      "live_text,t,Window_In=Win.Win,location=[0.3,0.9]\n"
      "return,d\nend ; FUN plot\n"
    "pro stv,S,T,V\n"
      "; procedure set-tag-value that does S.T=V\n"
      "sv=size(V)\n"
      "T=strupcase(T)\n"
      "TL=strupcase(tag_names(S))\n"
      "id=where(TL eq T)\n"
      "sz=[0,0,0]\n"
      "vd=n_elements(sv)-2\n"
      "type=sv[vd]\n"
      "if id(0) ge 0 then d=execute('sz=SIZE(S.'+T+')')\n"
      "if (sz(sz(0)+1) ne sv(sv(0)+1)) or (sz(0) ne sv(0)) $\n"
      "  or (sz(sz(0)+2) ne sv(sv(0)+2)) $\n"
      "  or type eq 8 then begin\n"
      " ES = ''\n"
      " for k=0,n_elements(TL)-1 do begin\n"
      "  case TL(k) of\n"
      "   T:\n"
      "   else: ES=ES+','+TL(k)+':S.'+TL(k)\n"
      "  endcase\n"
      " endfor\n"
      " d=execute('S={'+T+':V'+ES+'}')\n"
      "endif else d=execute('S.'+T+'=V')\n"
      "end ; PRO stv\n"
    "function %7$s,plot=plot\n"
      "; %4$s function issued from McStas on %5$s\n" 
      "; McStas simulation %2$s: %3$s\n"
      "; import using s=%7$s(/plot)\n"
      "if keyword_set(plot) then p=1 else p=0\n"
      "%7$s={Format:'%4$s',URL:'http://neutron.risoe.dk',"
      "Editor:'%6$s',$\n"
      "Creator:'%2$s McStas " MCSTAS_VERSION " simulation',$\n"
      "Date:%8$li,"
      "File:'%3$s'}\n",
    "stv,%7$s,'EndDate',%8$li ; for systime\nreturn, %7$s\nend\n",
    "; Section %2$s [%3$s] (level %7$d)\n"
      "%1$s%4$s={class:'%2$s'}\n",
    "%1$sstv,%6$s,'%4$s',%4$s\n",
    "%1$sstv,%2$s,'%3$s','%4$s'\n",
    "%1$sstv,%2$s,'func','%2$s' & data=[ ",
    "%1$sif single_file ne 0 then begin errors=[ ",
    "%1$sif single_file ne 0 then begin events=[ ",
    " ]\n%1$sif size(data,/type) eq 7 then single_file=0 else single_file=1\n"
    "%1$sstv,%2$s,'data',data & data=0 & %2$s=mcplot_inline(%2$s,p)\n",
    " ]\n%1$sstv,%2$s,'errors',reform(errors,%14$d,%15$d,/over) & errors=0\n%1$sendif\n",
    " ]\n%1$sstv,%2$s,'events',reform(events,%14$d,%15$d,/over) & events=0\n%1$sendif\n\n"},
  { "XML", "xml",
    "<?xml version=\"1.0\" ?>\n<!--\n"
      "URL:    http://www.neutron.anl.gov/nexus/xml/NXgroup.xml\n"
      "Editor: %6$s\n"
      "Creator:%2$s McStas " MCSTAS_VERSION " [neutron.risoe.dk].\n"
      "Date:   Simulation started (%8$li) %5$s\n"
      "File:   %3$s\n-->\n"
      "<NX%7$s file_name=\"%3$s\" file_time=\"%5$s\" user=\"%6$s\">\n"
        "<NXentry name=\"McStas " MCSTAS_VERSION "\"><start_time>%5$s</start_time>\n",
    "<end_time>%5$s</end_time></NXentry></NX%7$s>\n<!-- EndDate:%5$s -->\n",
    "%1$s<NX%2$s name=\"%3$s\">\n",
    "%1$s</NX%2$s>\n",
    "%1$s<%3$s>%4$s</%3$s>\n",
    "%1$s<%6$s long_name=\"%5$s\" axis=\"1\" primary=\"1\" min=\"%17$g\""
        " max=\"%18$g\" dims=\"%14$d\" range=\"1\"></%6$s>\n"
      "%1$s<%8$s long_name=\"%7$s\" axis=\"2\" primary=\"1\" min=\"%19$g\""
        " max=\"%20$g\" dims=\"%15$d\" range=\"1\"></%8$s>\n"
      "%1$s<%10$s long_name=\"%9$s\" axis=\"3\" primary=\"1\" min=\"%21$g\""
        " max=\"%22$g\" dims=\"%16$d\" range=\"1\"></%10$s>\n"
      "%1$s<data long_name=\"%3$s\" signal=\"1\"  axis=\"[%6$s,%8$s,%10$s]\" file_name=\"%4$s\">",
    "%1$s<errors>", "%1$s<monitor>",
    "%1$s</data>\n", "%1$s</errors>\n", "%1$s</monitor>\n"},
  { "HTML", "html",
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD %5$s//EN\"\n"
      "\"http://www.w3.org/TR/html4/strict.dtd\">\n"
      "<HTML><HEAD><META name=\"Author\" content=\"%7$s\">\n"
      "<META name=\"Creator\" content=\"%2$s McStas " MCSTAS_VERSION " [neutron.risoe.dk] simulation\">\n"
      "<META name=\"Date\" content=\"%5$s\">\n"
      "<TITLE>[McStas %2$s]%3$s</TITLE></HEAD>\n"
      "<BODY><h1><a name=\"%7$s\">"
        "McStas simulation %2$s: %3$s</a></h1><br>\n"
        "This simulation report was automatically created by"
        " <a href=\"http://neutron.risoe.dk/\"><i>McStas " MCSTAS_VERSION "</i></a><br>\n"
        "<pre>User:   %6$s<br>\n"
        "%1$sCreator: <a href=\"%2$s\">%2$s</a> McStas simulation<br>\n"
        "%1$sDate:    (%8$li) %5$s<br></pre>\n",
    "<b>EndDate: </b>(%8$li) %5$s<br></BODY></HTML>\n",
    "%1$s<h%7$d><a name=\"%3$s\">%2$s %3$s</a></h%7$d> "
      "[child of <a href=\"#%5$s\">%5$s</a>]<br>\n"
      "%1$sAssociated <a href=\"%3$s\">data file %3$s</a><br>\n"
        "%1$sAssociated <a href=\"%3$s.png\">%2$s image %3$s.png<br> (when available)\n"
        "%1$s<img src=\"%3$s.png\" alt=\"%2$s %3$s image\" width=100></a><br>\n",
    "[end of <a href=\"#%3$s\">%2$s %3$s</a>]<br>\n",
    "%1$s<b>%3$s: </b>%4$s<br>\n",
    "%1$s<b>DATA</b><br>\n",
      "%1$s<b>ERRORS</b><br>\n","%1$s<b>EVENTS</b><br>\n",
      "%1$sEnd of DATA<br>\n", "%1$sEnd of ERRORS<br>\n", "%1$sEnd of EVENTS<br>\n"},
  { "OpenGENIE", "gcl",
    "PROCEDURE get_%7$s\n"
      "RESULT %7$s\n"
      "# %4$s procedure issued from McStas on %5$s\n"
      "# McStas simulation %2$s: %3$s" MC_PATHSEP_S "%4$s\n"
      "# import data using s=get_%7$s();\n"
      "%7$s = fields();\n"
      "%7$s.Format =\"%4$s\";\n"
      "%7$s.URL    =\"http://neutron.risoe.dk\";\n"
      "%7$s.Editor =\"%6$s\";\n"
      "%7$s.Creator=\"%2$s McStas " MCSTAS_VERSION " simulation\";\n"
      "%7$s.Date   =%8$li;\n"
      "%7$s.File   =\"%3$s\";\n",
    "%7$s.EndDate=%8$li;\nENDPROCEDURE\n",
    "# Section %2$s [%3$s] (level %7$d)\n"
      "%1$s%4$s = fields(); %4$s.class = \"%2$s\";",
    "%1$s%6$s.%4$s = %4$s; free \"%4$s\";\n",
    "%1$s%2$s.%3$s = \"%4$s\";\n",
    "%1$s%2$s.func=\"get_%2$s\";\n%1$s%2$s.data = [ ",
    "%1$sIF (single_file = 1); %2$s.errors = [ ",
    "%1$sIF (single_file = 1); %2$s.ncount = [ ",
    " ] array(%14$d,%15$d); # end of data\nIF (length(%2$s.data) = 0); single_file=0; ELSE single_file=1; ENDIF\n%2$s=mcplot_inline(%2$s,p);\n",
    " ] array(%14$d,%15$d); # end of errors\nENDIF\n",
    " ] array(%14$d,%15$d); # end of ncount\nENDIF\n"},
  { "Octave", "m",
    "function mc_%7$s = get_%7$s(p)\n"
      "%% %4$s function issued from McStas on %5$s\n"
      "%% McStas simulation %2$s: %3$s\n"
      "%% import data using s=%7$s('plot');\n"
      "if nargin > 0, p=1; else p=0; end\n"
      "mc_%7$s.Format ='%4$s';\n"
      "mc_%7$s.URL    ='http://neutron.risoe.dk';\n"
      "mc_%7$s.Editor ='%6$s';\n"
      "mc_%7$s.Creator='%2$s McStas " MCSTAS_VERSION " simulation';\n"
      "mc_%7$s.Date   =%8$li; %% for datestr\n"
      "mc_%7$s.File   ='%3$s';\n",
    "mc_%7$s.EndDate=%8$li; %% for datestr\nendfunction\n"
      "if exist('mcload_inline'), return; end\n"
      "function d=mcload_inline(d)\n"
      "%% local inline function to load data\n"
      "S=d.type; eval(['S=[ ' S(10:(length(S)-1)) ' ];']);\n"
      "if isempty(d.data)\n"
      " if ~length(findstr(d.format, 'binary'))\n"
      "  source(d.filename);p=d.parent;\n"
      "  eval(['d=get_',d.func,';']);d.parent=p;\n"
      " else\n"
      "  if length(findstr(d.format, 'float')), t='float';\n"
      "  elseif length(findstr(d.format, 'double')), t='double';\n"
      "  else return; end\n"
      "  if length(S) == 1, S=[S 1]; end\n"
      "  fid=fopen(d.filename, 'r');\n"
      "  pS = prod(S);\n"
      "  x = fread(fid, 3*pS, t);\n"
      "  d.data  =reshape(x(1:pS), S);\n"
      "  if prod(size(x)) >= 3*pS,\n"
      "  d.errors=reshape(x((pS+1):(2*pS)), S);\n"
      "  d.events=reshape(x((2*pS+1):(3*pS)), S);end\n"
      "  fclose(fid);\n"
      "  return\n"
      " end\n"
      "end\n"
      "return;\nendfunction\n\n"
      "function d=mcplot_inline(d,p)\n"
      "%% local inline function to plot data\n"
      "if isempty(findstr(d.type,'0d')), d=mcload_inline(d); end\nif ~p, return; end;\n"
      "eval(['l=[',d.xylimits,'];']); S=size(d.data);\n"
      "t1=['[',d.parent,'] ',d.filename,': ',d.title];t = strcat(t1,['  ',d.variables,'=[',d.values,']'],['  ',d.signal],['  ',d.statistics]);\n"
      "disp(t);\n"
      "if ~isempty(findstr(d.type,'0d')), return; end\n"
      "xlabel(d.xlabel); ylabel(d.ylabel); title(t);"
      "figure; if ~isempty(findstr(d.type,'2d'))\n"
      "d.x=linspace(l(1),l(2),S(2)); d.y=linspace(l(3),l(4),S(1));\n"
      "mesh(d.x,d.y,d.data);\n"
      "else\nd.x=linspace(l(1),l(2),max(S));\nplot(d.x,d.data);end\nendfunction\n",
    "%% Section %2$s [%3$s] (level %7$d)\n"
      "mc_%4$s.class = '%2$s';",
    "mc_%6$s.mc_%4$s = mc_%4$s;\n",
    "%1$smc_%2$s.%3$s = '%4$s';\n",
    "%1$smc_%2$s.func='%2$s';\n%1$smc_%2$s.data = [ ",
    "%1$serrors = [ ",
    "%1$sevents = [ ",
    " ]; %% end of data\nif length(mc_%2$s.data) == 0, single_file=0; else single_file=1; end\nmc_%2$s=mcplot_inline(mc_%2$s,p);\n",
    " ]; %% end of errors\nif single_file, mc_%2$s.errors=errors; end\n",
    " ]; %% end of events\nif single_file, mc_%2$s.events=events; end\n"}
    };




/* file i/o handling ======================================================== */
/* opens a new file within mcdirname if non NULL */
/* if mode is non 0, then mode is used, else mode is 'w' */

FILE *
mcnew_file(const char *name, const char *mode)
{
  int dirlen;
  char *mem;
  FILE *file;

  if (!name || strlen(name) == 0) return(NULL);
  
  dirlen = mcdirname ? strlen(mcdirname) : 0;
  mem = (char *)malloc(dirlen + 1 + strlen(name) + 1);
  if(!mem)
  {
    fprintf(stderr, "Error: Out of memory (mcnew_file)\n");
    exit(1);
  }
  strcpy(mem, "");
  if(dirlen)
  {
    strcat(mem, mcdirname);
    if(mcdirname[dirlen - 1] != MC_PATHSEP_C &&
       name[0] != MC_PATHSEP_C)
      strcat(mem, MC_PATHSEP_S);
  }
  strcat(mem, name);
  file = fopen(mem, (mode ? mode : "w"));
  if(!file)
    fprintf(stderr, "Warning: could not open output file '%s'\n", mem);
  free(mem);
  return file;
} /* mcnew_file */

/* mcvalid_name: makes a valid string for variable names.
 * copy 'original' into 'valid', replacing invalid characters by '_'
 * char arrays must be pre-allocated. n can be 0, or the maximum number of
 * chars to be copied/checked
 */
static char *mcvalid_name(char *valid, const char *original, int n)
{
  long i;
  
  
  if (original == NULL || strlen(original) == 0) 
  { strcpy(valid, "noname"); return(valid); }
  if (n <= 0) n = strlen(valid);
  
  if (n > strlen(original)) n = strlen(original);
  strncpy(valid, original, n);
  
  for (i=0; i < n; i++)
  { 
    if ( (valid[i] > 122) 
      || (valid[i] < 32) 
      || (strchr("!\"#$%&'()*+,-.:;<=>?@[\\]^`/ ", valid[i]) != NULL) )
    {
      if (i) valid[i] = '_'; else valid[i] = 'm';
    }
  }
  valid[i] = '\0';
  
  return(valid);
} /* mcvalid_name */

#if defined(NL_ARGMAX) || defined(WIN32)
static int pfprintf(FILE *f, const char *fmt, const char *fmt_args, ...)
{
/* this function 
1- look for the maximum %d$ field in fmt
2- looks for all %d$ fields up to max in fmt and set their type (next alpha)
3- retrieve va_arg up to max, and save pointer to arg in local arg array
4- use strchr to split around '%' chars, until all pieces are written

usage: just as fprintf, but with (char *)fmt_args being the list of arg type
 */
  
  #define MyNL_ARGMAX 50
  char  *fmt_pos;
  
  char *arg_char[MyNL_ARGMAX];
  int   arg_int[MyNL_ARGMAX];
  long  arg_long[MyNL_ARGMAX];
  double arg_double[MyNL_ARGMAX];
  
  char *arg_posB[MyNL_ARGMAX];  /* position of '%' */
  char *arg_posE[MyNL_ARGMAX];  /* position of '$' */
  char *arg_posT[MyNL_ARGMAX];  /* position of type */
  
  int   arg_num[MyNL_ARGMAX];   /* number of argument (between % and $) */
  int   this_arg=0;
  int   arg_max=0;
  va_list ap;

  if (!f || !fmt_args || !fmt) return(-1);
  for (this_arg=0; this_arg<MyNL_ARGMAX;  arg_num[this_arg++] =0); this_arg = 0;
  fmt_pos = fmt;
  while(1)  /* analyse the format string 'fmt' */
  {
    char *tmp;
    
    arg_posB[this_arg] = (char *)strchr(fmt_pos, '%');
    tmp = arg_posB[this_arg];
    if (tmp)
    {
      arg_posE[this_arg] = (char *)strchr(tmp, '$');
      if (arg_posE[this_arg] && tmp[1] != '%')
      {
        char  this_arg_chr[10];
        char  printf_formats[]="dliouxXeEfgGcs\0";
        
        /* extract positional argument index %*$ in fmt */
        strncpy(this_arg_chr, arg_posB[this_arg]+1, arg_posE[this_arg]-arg_posB[this_arg]-1);
        this_arg_chr[arg_posE[this_arg]-arg_posB[this_arg]-1] = '\0';
        arg_num[this_arg] = atoi(this_arg_chr);
        if (arg_num[this_arg] <=0 || arg_num[this_arg] >= MyNL_ARGMAX)
          return(-fprintf(stderr,"pfprintf: invalid positional argument number (<=0 or >=%i) %s.\n", MyNL_ARGMAX, arg_posB[this_arg]));
        /* get type of positional argument: follows '%' -> arg_posE[this_arg]+1 */
        fmt_pos = arg_posE[this_arg]+1;
        if (!strchr(printf_formats, fmt_pos[0])) 
          return(-fprintf(stderr,"pfprintf: invalid positional argument type (%c != expected %c).\n", fmt_pos[0], fmt_args[arg_num[this_arg]-1]));
        if (fmt_pos[0] == 'l' && fmt_pos[1] == 'i') fmt_pos++;
        arg_posT[this_arg] = fmt_pos;
        /* get next argument... */
        this_arg++;
      } 
      else
      {
        if  (tmp[1] != '%')
          return(-fprintf(stderr,"pfprintf: must use only positional arguments (%s).\n", arg_posB[this_arg]));
        else fmt_pos = arg_posB[this_arg]+2;  /* found %% */
      }
    } else 
      break;  /* no more % argument */
  }
  arg_max = this_arg;
  /* get arguments from va_arg list, according to their type */
  va_start(ap, fmt_args);
  for (this_arg=0; this_arg<strlen(fmt_args); this_arg++)
  {
    
    switch(fmt_args[this_arg])
    {
      case 's':                       /* string */
              arg_char[this_arg] = va_arg(ap, char *);
              break;
      case 'd':
      case 'i':  
      case 'c':                     /* int */
              arg_int[this_arg] = va_arg(ap, int);
              break;
      case 'l':                       /* int */
              arg_long[this_arg] = va_arg(ap, long int);
              break;
      case 'f': 
      case 'g': 
      case 'G':                      /* double */
              arg_double[this_arg] = va_arg(ap, double);
              break;
      default: fprintf(stderr,"pfprintf: argument type is not implemented (arg %%%i$ type %c).\n", this_arg+1, fmt_args[this_arg]);
    }
  }
  va_end(ap);
  /* split fmt string into bits containing only 1 argument */
  fmt_pos = fmt;
  for (this_arg=0; this_arg<arg_max; this_arg++)
  {
    char *fmt_bit;
    int   arg_n;
    
    if (arg_posB[this_arg]-fmt_pos>0)
    {
      fmt_bit = (char*)malloc(arg_posB[this_arg]-fmt_pos+10);
      if (!fmt_bit) return(-fprintf(stderr,"pfprintf: not enough memory.\n"));
      strncpy(fmt_bit, fmt_pos, arg_posB[this_arg]-fmt_pos);
      fmt_bit[arg_posB[this_arg]-fmt_pos] = '\0';
      fprintf(f, fmt_bit); /* fmt part without argument */
    } else 
    {
      fmt_bit = (char*)malloc(10);
      if (!fmt_bit) return(-fprintf(stderr,"pfprintf: not enough memory.\n"));
    }
    arg_n = arg_num[this_arg]-1; /* must be >= 0 */
    strcpy(fmt_bit, "%");
    strncat(fmt_bit, arg_posE[this_arg]+1, arg_posT[this_arg]-arg_posE[this_arg]);
    fmt_bit[arg_posT[this_arg]-arg_posE[this_arg]+1] = '\0';
    
    switch(fmt_args[arg_n])
    {
      case 's': fprintf(f, fmt_bit, arg_char[arg_n]);
                break;
      case 'd': 
      case 'i':
      case 'c':                      /* int */
              fprintf(f, fmt_bit, arg_int[arg_n]);
              break;
      case 'l':                       /* long */
              fprintf(f, fmt_bit, arg_long[arg_n]);
              break;
      case 'f': 
      case 'g': 
      case 'G':                       /* double */
              fprintf(f, fmt_bit, arg_double[arg_n]);
              break;
    }
    fmt_pos = arg_posT[this_arg]+1;
    if (this_arg == arg_max-1)
    { /* add eventual leading characters for last parameter */
      if (fmt_pos < fmt+strlen(fmt))
        fprintf(f, "%s", fmt_pos);
    }
    if (fmt_bit) free(fmt_bit);
    
  }
  return(this_arg);
}
#else
static int pfprintf(FILE *f, char *fmt, char *fmt_args, ...)
{ /* wrapper to standard fprintf */
  va_list ap;
  int tmp;

  va_start(ap, fmt_args);
  tmp=vfprintf(f, fmt, ap);
  va_end(ap);
  return(tmp);
}
#endif

/* mcfile_header: output header/footer using specific file format.
 * outputs, in file 'name' having preallocated 'f' handle, the format Header
 * 'part' may be 'header' or 'footer' depending on part to write
 * if name == NULL, ignore function (no header/footer output)
 */
static int mcfile_header(FILE *f, struct mcformats_struct format, const char *part, const char *pre, const char *name, const char *parent)
{
  char user[64];
  char date[64];
  char *HeadFoot;
  long date_l; /* date as a long number */
  time_t t;
  char valid_parent[256];
  char instrname[256];
  char file[256];
  
  if(!f)
    return (-1);
    
  time(&t);
  
  if (part && !strcmp(part,"footer")) 
  {
    HeadFoot = format.Footer;
    date_l = (long)t;;
  }
  else 
  {
    HeadFoot = format.Header;
    date_l = mcstartdate;
  }
  t = (time_t)date_l;
    
  if (!strlen(HeadFoot) || (!name)) return (-1);

  sprintf(file,"%s",name);
  sprintf(user,"%s on %s", getenv("USER"), getenv("HOST"));
  sprintf(instrname,"%s (%s)", mcinstrument_name, mcinstrument_source);
  strncpy(date, ctime(&t), 64); 
  if (strlen(date)) date[strlen(date)-1] = '\0';
  
  if (parent && strlen(parent)) mcvalid_name(valid_parent, parent, 256);
  else strcpy(valid_parent, "root");
  
  return(pfprintf(f, HeadFoot, "sssssssl", 
    pre,                  /* %1$s */
    instrname,            /* %2$s */
    file,                 /* %3$s */
    format.Name,          /* %4$s */
    date,                 /* %5$s */
    user,                 /* %6$s */
    valid_parent,         /* %7$s*/
    date_l));             /* %8$li */
} /* mcfile_header */


void remove_quotes( char * s, const struct mcformats_struct & format )
{
  /* remove quote chars in ss */
  if (strstr(format.Name, "Scilab") || strstr(format.Name, "Matlab") || strstr(format.Name, "IDL"))
    for(size_t i = 0; i < strlen(s); i++)
      if (s[i] == '"' || s[i] == '\'') s[i] = ' ';  
}


/* mcfile_tag: output tag/value using specific file format.
 * outputs, in file with 'f' handle, a tag/value pair.
 * if name == NULL, ignore function (no section definition)
 */
static int mcfile_tag(FILE *f, struct mcformats_struct format, const char *pre, const char *section, const char *name, const char *value)
{
  char valid_section[256];
  
  if (!strlen(format.AssignTag) || (!name) || (!f)) return(-1);
  
  mcvalid_name(valid_section, section, 256);
  
  return(pfprintf(f, format.AssignTag, "ssss",
    pre,          /* %1$s */
    valid_section,/* %2$s */
    name,         /* %3$s */
    value));      /* %4$s */
} /* mcfile_tag */

/* mcfile_section: output section start/end using specific file format.
 * outputs, in file 'name' having preallocated 'f' handle, the format Section.
 * 'part' may be 'begin' or 'end' depending on section part to write
 * 'type' may be e.g. 'instrument','simulation','component','data'
 * if name == NULL, ignore function (no section definition)
 * the prefix 'pre' is automatically idented/un-indented (pre-allocated !)
 */
 
static int mcfile_section(FILE *f, struct mcformats_struct format, char *part, char *pre, char *name, char *type, char *parent, int level) 
{
  char *Section;
  char valid_name[256];
  char valid_parent[256];
  int  ret;
  
  if(!f)
    return (-1);
  
  if (part && !strcmp(part,"end")) Section = format.EndSection;
  else Section = format.BeginSection;
    
  if (!strlen(Section) || (!name)) return (-1);
  
  mcvalid_name(valid_name, name, 256);
  if (parent && strlen(parent)) mcvalid_name(valid_parent, parent, 256);
  else strcpy(valid_parent, "root");
  
  if (!strcmp(part,"end") && pre) 
  {
    if (strlen(pre) <= 2) strcpy(pre,"");
    else pre[strlen(pre)-2]='\0'; 
  }
  
  ret = pfprintf(f, Section, "ssssssl",
    pre,          /* %1$s */
    type,         /* %2$s */
    name,         /* %3$s */
    valid_name,   /* %4$s */
    parent,       /* %5$s */
    valid_parent, /* %6$s */
    level);       /* %7$li */
  
  if (!strcmp(part,"begin")) 
  {
    strcat(pre,"  ");
    if (name && strlen(name)) 
      mcfile_tag(f, format, pre, name, "name", name);
    if (parent && strlen(parent)) 
      mcfile_tag(f, format, pre, name, "parent", parent);
  }
  
  
  return(ret);
} /* mcfile_section */

static void mcinfo_instrument(FILE *f, struct mcformats_struct format, 
  char *pre, char *name)
{
  char Value[1300] = "";
  int  i;
  
  if (!f) return;

  for(i = 0; i < mcnumipar; i++)
  {
    char ThisParam[256];
    if (strlen(mcinputtable[i].name) > 200) break;
    /*
    sprintf(ThisParam, " %s(%s)", mcinputtable[i].name,
            (*mcinputtypes[mcinputtable[i].type].parminfo)
                (mcinputtable[i].name));
    strcat(Value, ThisParam);
    if (strlen(Value) > 1024) break;
    */
  }

  char tmp[256];

  remove_quotes( tmp, format );
  mcfile_tag(f, format, pre, name, "Parameters", tmp);

  mcfile_tag(f, format, pre, name, "Source", mcinstrument_source);
  mcfile_tag(f, format, pre, name, "Trace_enabled", mctraceenabled ? "yes" : "no");
  mcfile_tag(f, format, pre, name, "Default_main", mcdefaultmain ? "yes" : "no");
  mcfile_tag(f, format, pre, name, "Embedded_runtime", 
#ifdef MC_EMBEDDED_RUNTIME
         "yes"
#else
         "no"
#endif
         );
} /* mcinfo_instrument */

void mcinfo_simulation(FILE *f, struct mcformats_struct format, 
  char *pre, char *name) 
{
  int i;
  double run_num, ncount;
  time_t t;
  char Value[256];
  
  if (!f) return;
    
  run_num = mcget_run_num();
  ncount  = mcget_ncount();
  time(&t);
  strncpy(Value, ctime(&t), 256); if (strlen(Value)) Value[strlen(Value)-1] = '\0';

  remove_quotes( Value, format );
  mcfile_tag(f, format, pre, name, "Date", Value); 

  if (run_num == 0 || run_num == ncount) sprintf(Value, "%g", ncount);
  else sprintf(Value, "%g/%g", run_num, ncount);
  mcfile_tag(f, format, pre, name, "Ncount", Value);
  mcfile_tag(f, format, pre, name, "Trace", mcdotrace ? "yes" : "no");
  mcfile_tag(f, format, pre, name, "Gravitation", mcgravitation ? "yes" : "no");
  if(mcseed)
  {
    sprintf(Value, "%ld", mcseed);
    mcfile_tag(f, format, pre, name, "Seed", Value);
  }
  if (strstr(format.Name, "McStas"))
  {
    for(i = 0; i < mcnumipar; i++)
    {
      /*
      if (run_num || (mcinputtable[i].val && strlen(mcinputtable[i].val))) {
        (*mcinputtypes[mcinputtable[i].type].printer)(Value, mcinputtable[i].par);
        fprintf(f, "%sParam: %s=%s", pre, mcinputtable[i].name, Value);
        fprintf(f, "\n");
      }
      */
    }   
  }
  else
  {
    mcfile_section(f, format, "begin", pre, "parameters", "parameters", name, 3);
    for(i = 0; i < mcnumipar; i++)
    {
      /*
      (*mcinputtypes[mcinputtable[i].type].printer)(Value, mcinputtable[i].par);
      mcfile_tag(f, format, pre, "parameters", mcinputtable[i].name, Value);
      */
    }  
    mcfile_section(f, format, "end", pre, "parameters", "parameters", name, 3);
  }
} /* mcinfo_simulation */

static void mcinfo_data(FILE *f, struct mcformats_struct format, 
			const char *pre, const char *parent, const char *title,
			int m, int n, int p,
			const char *xlabel, const char *ylabel, const char *zlabel, 
			const char *xvar, const char *yvar, const char *zvar, 
			double x1, double x2, double y1, double y2, double z1, double z2, 
			const char *filename,
			double *p0, double *p1, double *p2, char istransposed)
{
  char type[256];
  char stats[256];
  char vars[256];
  char signal[256];
  char values[256];
  char limits[256];
  char lim_field[10];
  char c[32];
  double run_num, ncount;
  char ratio[256];
  
  double sum_xz  = 0;
  double sum_yz  = 0;
  double sum_z   = 0;
  double sum_y   = 0;
  double sum_x   = 0;
  double sum_x2z = 0;
  double sum_y2z = 0;
  double min_z   = 0;
  double max_z   = 0;
  double fmon_x=0, smon_x=0, fmon_y=0, smon_y=0, mean_z=0;
  double Nsum=0;
  double P2sum=0;
  
  int    i,j;
  
  if (!f || m*n*p == 0) return;
  
  if (p1)
  {
    min_z   = p1[0];
    max_z   = min_z;
    for(j = 0; j < n*p; j++)
    {
      for(i = 0; i < m; i++)
      {
        double x,y,z;
        double N, E;
        long index;

        if (!istransposed) index = i*n*p + j;
        else index = i+j*m;
        if (p0) N = p0[index];
        if (p2) E = p2[index];

        if (m) x = x1 + (i + 0.5)/m*(x2 - x1); else x = 0;
        if (n) y = y1 + (j + 0.5)/n/p*(y2 - y1); else y = 0;
        z = p1[index];
        sum_xz += x*z;
        sum_yz += y*z;
        sum_x += x;
        sum_y += y;
        sum_z += z;
        sum_x2z += x*x*z;
        sum_y2z += y*y*z;
        if (z > max_z) max_z = z;
        if (z < min_z) min_z = z;

        Nsum += p0 ? N : 1;
        P2sum += p2 ? E : z*z;
      }
    }
    if (sum_z && n*m*p)
    {
      fmon_x = sum_xz/sum_z; 
      fmon_y = sum_yz/sum_z;
      smon_x = sqrt(sum_x2z/sum_z-fmon_x*fmon_x);
      smon_y = sqrt(sum_y2z/sum_z-fmon_y*fmon_y);
      mean_z = sum_z/n/m/p;
    }
  }
  
  if (m*n*p == 1) 
  { strcpy(type, "array_0d"); strcpy(stats, ""); }
  else if (n == 1 || m == 1) 
  { if (m == 1) {m = n; n = 1; }
    sprintf(type, "array_1d(%d)", m); 
    sprintf(stats, "X0=%g; dX=%g;", fmon_x, smon_x); }
  else  
  { if (p == 1) sprintf(type, "array_2d(%d, %d)", m, n); 
    else sprintf(type, "array_3d(%d, %d, %d)", m, n, p); 
    sprintf(stats, "X0=%g; dX=%g; Y0=%g; dY=%g;", fmon_x, smon_x, fmon_y, smon_y); }
  strcpy(c, "I ");
  if (zvar && strlen(zvar)) strncpy(c, zvar,32);
  else if (yvar && strlen(yvar)) strncpy(c, yvar,32);
  else if (xvar && strlen(xvar)) strncpy(c, xvar,32);
  else strncpy(c, xvar,32);
  if (m == 1 || n == 1) sprintf(vars, "%s %s %s_err N", xvar, c, c);
  else sprintf(vars, "%s %s_err N", c, c);

  run_num = mcget_run_num();
  ncount  = mcget_ncount();
  sprintf(ratio, "%g/%g", run_num, ncount);
  
  mcfile_tag(f, format, pre, parent, "type", type);
  mcfile_tag(f, format, pre, parent, "Source", mcinstrument_source);
  if (parent) mcfile_tag(f, format, pre, parent, (strstr(format.Name,"McStas") ? "component" : "parent"), parent);
  if (title) mcfile_tag(f, format, pre, parent, "title", title);
  mcfile_tag(f, format, pre, parent, "ratio", ratio);
  if (filename) {
    mcfile_tag(f, format, pre, parent, "filename", filename);
    mcfile_tag(f, format, pre, parent, "format", format.Name);
  } else mcfile_tag(f, format, pre, parent, "filename", "");
  
  if (p1)
  {
    if (n*m*p > 1) 
    {
      sprintf(signal, "Min=%g; Max=%g; Mean= %g;", min_z, max_z, mean_z); 
      if (y1 == 0 && y2 == 0) { y1 = min_z; y2 = max_z;}
      else if (z1 == 0 && z2 == 0) { z1 = min_z; z2 = max_z;}
    } else strcpy(signal, "");

    mcfile_tag(f, format, pre, parent, "statistics", stats);
    mcfile_tag(f, format, pre, parent, "signal", signal);

    sprintf(values, "%g %g %g", sum_z, mcestimate_error(Nsum, sum_z, P2sum), Nsum);
    mcfile_tag(f, format, pre, parent, "values", values);
  }
  strcpy(lim_field, "xylimits");
  if (n*m > 1) 
  {
    mcfile_tag(f, format, pre, parent, "xvar", xvar);
    mcfile_tag(f, format, pre, parent, "yvar", yvar);
    mcfile_tag(f, format, pre, parent, "xlabel", xlabel);
    mcfile_tag(f, format, pre, parent, "ylabel", ylabel);
    if ((n == 1 || m == 1) && strstr(format.Name, "McStas"))
    {
      sprintf(limits, "%g %g", x1, x2);
      strcpy(lim_field, "xlimits");
    }
    else
    {
      mcfile_tag(f, format, pre, parent, "zvar", zvar);
      mcfile_tag(f, format, pre, parent, "zlabel", zlabel);
      sprintf(limits, "%g %g %g %g %g %g", x1, x2, y1, y2, z1, z2);
    }
  } else strcpy(limits, "0 0 0 0 0 0");
  mcfile_tag(f, format, pre, parent, lim_field, limits);
  mcfile_tag(f, format, pre, parent, "variables", vars);
} /* mcinfo_data */

/* main output function, works for 0d, 1d, 2d data */

void
mcsiminfo_init(FILE *f)
{
  char info_name[256];
  
  if (mcdisable_output_files) return;
  if (!f && (!mcsiminfo_name || !strlen(mcsiminfo_name))) return;
  if (!strchr(mcsiminfo_name,'.')) sprintf(info_name, "%s.%s", mcsiminfo_name, mcformat.Extension); else strcpy(info_name, mcsiminfo_name);
  if (!f) mcsiminfo_file = mcnew_file(info_name, "w");
  else mcsiminfo_file = f;
  if(!mcsiminfo_file)
    fprintf(stderr,
            "Warning: could not open simulation description file '%s'\n",
            info_name);
  else
  {
    char pre[20];
    int  ismcstas;
    char simname[1024];
    char root[10];
    
    strcpy(pre, "");
    ismcstas = (strstr(mcformat.Name, "McStas") != NULL);
    if (strstr(mcformat.Name, "XML") == NULL && strstr(mcformat.Name, "NeXus") == NULL) strcpy(root, "mcstas");
    else strcpy(root, "root");
    if (mcdirname) sprintf(simname, "%s%s%s", mcdirname, MC_PATHSEP_S, mcsiminfo_name); else sprintf(simname, "%s%s%s", ".", MC_PATHSEP_S, mcsiminfo_name);
    
    mcfile_header(mcsiminfo_file, mcformat, "header", pre, simname, root);
    mcfile_section(mcsiminfo_file, mcformat, "begin", pre, mcinstrument_name, "instrument", root, 1);
    mcinfo_instrument(mcsiminfo_file, mcformat, pre, mcinstrument_name);
    if (ismcstas) mcfile_section(mcsiminfo_file, mcformat, "end", pre, mcinstrument_name, "instrument", root, 1);
    mcfile_section(mcsiminfo_file, mcformat, "begin", pre, simname, "simulation", mcinstrument_name, 2);
    mcinfo_simulation(mcsiminfo_file, mcformat, pre, simname);
    if (ismcstas) mcfile_section(mcsiminfo_file, mcformat, "end", pre, simname, "simulation", mcinstrument_name, 2);
  }
} /* mcsiminfo_init */

void
mcsiminfo_close(void)
{
  if (mcdisable_output_files) return;
  if(mcsiminfo_file)
  {
    int  ismcstas;
    char simname[1024];
    char root[10];
    char pre[10];
    
    strcpy(pre, "  ");
    ismcstas = (strstr(mcformat.Name, "McStas") != NULL);
    if (mcdirname) sprintf(simname, "%s%s%s", mcdirname, MC_PATHSEP_S, mcsiminfo_name); else sprintf(simname, "%s%s%s", ".", MC_PATHSEP_S, mcsiminfo_name);
    if (strstr(mcformat.Name, "XML") == NULL && strstr(mcformat.Name, "NeXus") == NULL) strcpy(root, "mcstas"); else strcpy(root, "root");
    
    if (!ismcstas) 
    {
      mcfile_section(mcsiminfo_file, mcformat, "end", pre, simname, "simulation", mcinstrument_name, 2);
      mcfile_section(mcsiminfo_file, mcformat, "end", pre, mcinstrument_name, "instrument", root, 1);
    }
    mcfile_header(mcsiminfo_file, mcformat, "footer", pre, simname, root);
    
    if (mcsiminfo_file != stdout) fclose(mcsiminfo_file);
    mcsiminfo_file = NULL;
  }
} /* mcsiminfo_close */

/* mcfile_datablock: output a single data block using specific file format.
 * 'part' can be 'data','errors','ncount'
 * if y1 == y2 == 0 and McStas format, then stores as a 1D array with [I,E,N]
 * return value: 0=0d/2d, 1=1d
 * when !single_file, create independent data files, with header and data tags
 * if one of the dimensions m,n,p is negative, the data matrix will be written
 * after transposition of m/x and n/y dimensions
 */

static int mcfile_datablock(FILE *f, struct mcformats_struct format, 
			    const char *pre, const char *parent, const char *part,
			    double *p0, double *p1, double *p2, int m, int n, int p,
			    const char *xlabel, const char *ylabel, const char *zlabel, 
			    const char *title,
			    const char *xvar, const char *yvar, const char *zvar,
			    double x1, double x2, double y1, double y2, double z1, double z2, 
			    const char *filename, char istransposed)
{
  char *Begin;
  char *End;
  char valid_xlabel[64];
  char valid_ylabel[64];
  char valid_zlabel[64];
  char valid_parent[64];
  FILE *datafile= NULL;
  int  isdata=0;
  int  just_header=0;
  int  i,j, is1d;
  double Nsum=0, Psum=0, P2sum=0;
  char sec[256];
  char isdata_present;
  
  if (strstr(part,"data")) 
  { isdata = 1; Begin = format.BeginData; End = format.EndData; }
  if (strstr(part,"errors")) 
  { isdata = 2; Begin = format.BeginErrors; End = format.EndErrors; }
  if (strstr(part,"ncount")) 
  { isdata = 0; Begin = format.BeginNcount; End = format.EndNcount; }
  if (strstr(part, "begin")) just_header = 1;
  if (strstr(part, "end"))   just_header = 2;
  
  isdata_present=((isdata==1 && p1) || (isdata==2 && p2) || (isdata==0 && p0));
  
  is1d = ((m==1 || n==1) && strstr(format.Name,"McStas"));
  mcvalid_name(valid_xlabel, xlabel, 64);
  mcvalid_name(valid_ylabel, ylabel, 64);
  mcvalid_name(valid_zlabel, zlabel, 64);
  
  if (strstr(format.Name, "McStas") || !filename || strlen(filename) == 0) 
    mcvalid_name(valid_parent, parent, 64);
  else mcvalid_name(valid_parent, filename, 64);
  
  /* if normal or begin and part == data: output info_data (sim/data_file) */
  if (isdata == 1 && just_header != 2 && f)
  {
    mcinfo_data(f, format, pre, valid_parent, title, m, n, p,
          xlabel, ylabel, zlabel, xvar, yvar, zvar, 
          x1, x2, y1, y2, z1, z2, filename, p0, p1, p2, istransposed);
  }

  /* if normal or begin: begin part (sim/data file) */
  if (strlen(Begin) && just_header != 2 && f)
    pfprintf(f, Begin, "ssssssssssssslllgggggg",
      pre,          /* %1$s */
      valid_parent, /* %2$s */
      title,        /* %3$s */
      filename,     /* %4$s */
      xlabel,       /* %5$s */
      valid_xlabel, /* %6$s*/
      ylabel,       /* %7$s */
      valid_ylabel, /* %8$s */
      zlabel,       /* %9$s*/
      valid_zlabel, /* %10$s*/
      xvar,         /* %11$s */
      yvar,         /* %12$s */
      zvar,         /* %13$s */
      m,            /* %14$li */
      n,            /* %15$li */
      p,            /* %16$li */
      x1,           /* %17$g */
      x2,           /* %18$g */
      y1,           /* %19$g*/
      y2,           /* %20$g */
      z1,           /* %21$g */
      z2);          /* %22$g */
      
 /* if normal, and !single:
  *   open datafile, 
  *   if !ascii_only
  *     if data: write file header, 
  *     call datablock part+header(begin)
  * else data file = f
  */
  if (!mcsingle_file && just_header == 0)
  {
    /* if data: open new file for data else append for error/ncount */
    if (filename) datafile = mcnew_file(filename, 
      (isdata != 1 || strstr(format.Name, "append") ? "a" : "w"));
    else datafile = NULL;
    /* special case of IDL: can not have empty vectors. Init to 'empty' */
    if (strstr(format.Name, "IDL") && f) fprintf(f, "'external'");
    /* if data, start with root header plus tags of parent data */
    if (datafile && !mcascii_only) 
    { 
      char mode[32];
      if (isdata == 1) mcfile_header(datafile, format, "header",
          (strstr(format.Name, "McStas") ? "# " : ""), 
          filename, valid_parent); 
      sprintf(mode, "%s begin", part);
      /* write header+data block begin tags into datafile */
      mcfile_datablock(datafile, format, 
          (strstr(format.Name, "McStas") ? "# " : ""), 
          valid_parent, mode,
          p0, p1, p2, m, n, p,
          xlabel,  ylabel, zlabel, title,
          xvar, yvar, zvar,
          x1, x2, y1, y2, z1, z2, filename, istransposed);
      
      
    }
  }
  else if (just_header == 0)
  {
    if (strstr(format.Name, "McStas") && m*n*p>1 && f) 
    {
      if (is1d) sprintf(sec,"array_1d(%d)", m);
      else if (p==1) sprintf(sec,"array_2d(%d,%d)", m,n);
      else sprintf(sec,"array_3d(%d,%d,%d)", m,n,p);
      fprintf(f,"%sbegin %s\n", pre, sec);
      datafile = f;
    }
    if (mcsingle_file) datafile = f;
  }
  
  /* if normal: [data] in data file */
  /* do loops: 2 loops on m,n. */
  if (just_header == 0)
  {
    char eol_char[3];
    int  isIDL, isPython;
    int  isBinary=0;
    
    if (strstr(format.Name, "binary float")) isBinary=1;
    else if (strstr(format.Name, "binary double")) isBinary=2;
    isIDL    = (strstr(format.Name, "IDL") != NULL);
    isPython = (strstr(format.Name, "Python") != NULL);
    if (isIDL) strcpy(eol_char,"$\n"); else strcpy(eol_char,"\n");
         
    for(j = 0; j < n*p; j++)  /* loop on rows(y) */
    {
      if(datafile && !isBinary)
        fprintf(datafile,"%s", pre);
      for(i = 0; i < m; i++)  /* write all columns (x) */
      {
        double I=0, E=0, N=0;
        double value=0;
        long index;

        if (!istransposed) index = i*n*p + j;
        else index = i+j*m;
        if (p0) N = p0[index];
        if (p1) I = p1[index];
        if (p2) E = p2[index];

        Nsum += p0 ? N : 1;
        Psum += I;
        P2sum += p2 ? E : I*I;

        if (p0 && p1 && p2) E = mcestimate_error(N,I,E);
        if(datafile && !isBinary && isdata_present)
        {
          if (isdata == 1) value = I;
          else if (isdata == 0) value = N;
          else if (isdata == 2) value = E;
          if (is1d) 
          {
            double x;
            
            x = x1+(x2-x1)*(index)/(m*n*p);
            if (m*n*p > 1) fprintf(datafile, "%g %g %g %g\n", x, I, E, N);
          }
          else 
          {
            fprintf(datafile, "%g", value);
            if ((isIDL || isPython) && ((i+1)*(j+1) < m*n*p)) fprintf(datafile, ","); 
            else fprintf(datafile, " ");
          }
        }
      }
      if (datafile && !isBinary && isdata_present) fprintf(datafile, eol_char);
    } /* end 2 loops if not Binary */
    if (datafile && isBinary)
    {
      double *d=NULL;
      if (isdata==1) d=p1;
      else if (isdata==2) d=p2;
      else if (isdata==0) d=p0;

      if (d && isBinary == 1)  /* float */
      {
        float *s;
        s = (float*)malloc(m*n*p*sizeof(float));
        if (s) 
        {
          long    i, count;
          for (i=0; i<m*n*p; i++)
            { if (isdata != 2) s[i] = (float)d[i]; 
              else s[i] = (float)mcestimate_error(p0[i],p1[i],p2[i]); }
          count = fwrite(s, sizeof(float), m*n*p, datafile);
          if (count != m*n*p) fprintf(stderr, "McStas: error writing float binary file '%s' (%li instead of %li).\n", filename,count, (long)m*n*p);
          free(s);
        } else fprintf(stderr, "McStas: Out of memory for writing float binary file '%s'.\n", filename);
      }
      else if (d && isBinary == 2)  /* double */
      {
        long count;
        double *s=NULL;
        if (isdata == 2) 
        { 
          s = (double*)malloc(m*n*p*sizeof(double));
          if (s) { long i;
            for (i=0; i<m*n*p; i++)
              s[i] = (double)mcestimate_error(p0[i],p1[i],p2[i]);
            d = s;
          }
          else fprintf(stderr, "McStas: Out of memory for writing 'errors' part of double binary file '%s'.\n", filename);
        }
        count = fwrite(d, sizeof(double), m*n*p, datafile);
        if (isdata == 2 && s) free(s);
        if (count != m*n*p) fprintf(stderr, "McStas: error writing double binary file '%s' (%li instead of %li).\n", filename,count, (long)m*n*p);
      }
    } /* end if Binary */
  }
  if (strstr(format.Name, "McStas") || !filename || strlen(filename) == 0) 
    mcvalid_name(valid_parent, parent, 64);
  else mcvalid_name(valid_parent, filename, 64);
  /* if normal or end: end_data */
  if (strlen(End) && just_header != 1 && f)
  {
    pfprintf(f, End, "ssssssssssssslllgggggg",
      pre,          /* %1$s */
      valid_parent, /* %2$s */
      title,        /* %3$s */
      filename,     /* %4$s */
      xlabel,       /* %5$s */
      valid_xlabel, /* %6$s*/
      ylabel,       /* %7$s */
      valid_ylabel, /* %8$s */
      zlabel,       /* %9$s*/
      valid_zlabel, /* %10$s*/
      xvar,         /* %11$s */
      yvar,         /* %12$s */
      zvar,         /* %13$s */
      m,            /* %14$li */
      n,            /* %15$li */
      p,            /* %16$li */
      x1,           /* %17$g */
      x2,           /* %18$g */
      y1,           /* %19$g*/
      y2,           /* %20$g */
      z1,           /* %21$g */
      z2);          /* %22$g */
  }
      
 /* if normal and !single and datafile: 
  *   datablock part+footer
  *   write file footer
  *   close datafile
  */
  if (!mcsingle_file && just_header == 0)
  {
    char mode[32];

    if (datafile && datafile != f && !mcascii_only)
    {
      sprintf(mode, "%s end", part);
      /* write header+data block end tags into datafile */
      mcfile_datablock(datafile, format, 
          (strstr(format.Name, "McStas") ? "# " : ""),
          valid_parent, mode,
          p0, p1, p2, m, n, p,
          xlabel,  ylabel, zlabel, title,
          xvar, yvar, zvar,
          x1, x2, y1, y2, z1, z2, filename, istransposed);
      if ((isdata == 1 && is1d) || strstr(part,"ncount") || !p0 || !p2) /* either ncount, or 1d */
        if (!strstr(format.Name, "partial"))
          mcfile_header(datafile, format, "footer", 
          (strstr(format.Name, "McStas") ? "# " : ""),
          filename, valid_parent);
    }
    if (datafile) fclose(datafile); 
  }
  else
  {
    if (strstr(format.Name, "McStas") && just_header == 0 && m*n*p > 1) 
      fprintf(f,"%send %s\n", pre, sec);
  }
      
  /* set return value */      
  return(is1d);
} /* mcfile_datablock */

/* mcfile_data: output data/errors/ncounts using specific file format.
 * if McStas 1D then data is stored
 * as a long 1D array [p0, p1, p2] to reorder -> don't output err/ncount again.
 * if p1 or p2 is NULL then skip that part.
 */
static int mcfile_data(FILE *f, struct mcformats_struct format, 
  char *pre, char *parent, 
  double *p0, double *p1, double *p2, int m, int n, int p,
  char *xlabel, char *ylabel, char *zlabel, const char *title,
  char *xvar, char *yvar, char *zvar,
  double x1, double x2, double y1, double y2, double z1, double z2,
  char *filename, char istransposed)
{
  int is1d;
  
  /* return if f,n,m,p1 NULL */
  if ((m*n*p == 0) || !p1) return (-1);
  
  /* output data block */
  is1d = mcfile_datablock(f, format, pre, parent, "data",
    p0, p1, p2, m, n, p,
    xlabel,  ylabel, zlabel, title,
    xvar, yvar, zvar,
    x1, x2, y1, y2, z1, z2, filename, istransposed);
  /* return if 1D data */
  if (is1d) return(is1d);
  /* output error block and p2 non NULL */
  if (p0 && p2) mcfile_datablock(f, format, pre, parent, "errors",
    p0, p1, p2, m, n, p,
    xlabel,  ylabel, zlabel, title,
    xvar, yvar, zvar,
    x1, x2, y1, y2, z1, z2, filename, istransposed);
  /* output ncount block and p0 non NULL */
  if (p0 && p2) mcfile_datablock(f, format, pre, parent, "ncount",
    p0, p1, p2, m, n, p,
    xlabel,  ylabel, zlabel, title,
    xvar, yvar, zvar,
    x1, x2, y1, y2, z1, z2, filename, istransposed);
  
  return(is1d);
} /* mcfile_data */

double
mcdetector_out(char *cname, double p0, double p1, double p2, char *filename)
{
  printf("Detector: %s_I=%g %s_ERR=%g %s_N=%g",
         cname, p1, cname, mcestimate_error(p0,p1,p2), cname, p0);
  if(filename && strlen(filename))
    printf(" \"%s\"", filename);
  printf("\n");
  return(p0);
}

/* parent is the component name */

static double mcdetector_out_012D(struct mcformats_struct format, 
  char *pre, char *parent, const char *title,
  int m, int n,  int p,
  char *xlabel, char *ylabel, char *zlabel, 
  char *xvar, char *yvar, char *zvar, 
  double x1, double x2, double y1, double y2, double z1, double z2, 
  char *filename,
  double *p0, double *p1, double *p2)
{  
  char simname[512];
  int i,j;
  double Nsum=0, Psum=0, P2sum=0;
  FILE *local_f=NULL;
  char istransposed=0;
  
  if (m<0 || n<0 || p<0 || strstr(format.Name, "binary"))  /* do the swap once for all */
  { 
    double tmp1, tmp2;
    char   *lab;
    istransposed = 1; 
    
    i=m; m=abs(n); n=abs(i); p=abs(p); 
  }

  if (!strstr(format.Name,"partial")) local_f = mcsiminfo_file;
  if (mcdirname) sprintf(simname, "%s%s%s", mcdirname, MC_PATHSEP_S, mcsiminfo_name); else sprintf(simname, "%s%s%s", ".", MC_PATHSEP_S, mcsiminfo_name);
  
  if (!mcdisable_output_files)
  {
  
    mcfile_section(local_f, format, "begin", pre, parent, "component", simname, 3);
    mcfile_section(local_f, format, "begin", pre, filename, "data", parent, 4);
    mcfile_data(local_f, format, 
      pre, parent, 
      p0, p1, p2, m, n, p,
      xlabel, ylabel, zlabel, title,
      xvar, yvar, zvar, 
      x1, x2, y1, y2, z1, z2, filename, istransposed);

    mcfile_section(local_f, format, "end", pre, filename, "data", parent, 4);
    mcfile_section(local_f, format, "end", pre, parent, "component", simname, 3);
  }

  if (local_f || mcdisable_output_files)
  {
    for(j = 0; j < n*p; j++)
    {
      for(i = 0; i < m; i++)
      {
        double N,I,E;
        int index;
        if (!istransposed) index = i*n*p + j;
        else index = i+j*m;
        if (p0) N = p0[index];
        if (p1) I = p1[index];
        if (p2) E = p2[index];

        Nsum += p0 ? N : 1;
        Psum += I;
        P2sum += p2 ? E : I*I;
      }
    }
    /* give 0D detector output. */
    mcdetector_out(parent, Nsum, Psum, P2sum, filename);
  }
  return(Psum);
} /* mcdetector_out_012D */

void mcheader_out(FILE *f,char *parent,
		  int m, int n, int p,
		  const char *xlabel, const char *ylabel, const char *zlabel,
		  const char *title,
		  const char *xvar, const char *yvar, const char *zvar,
		  double x1, double x2, double y1, double y2, double z1, double z2, 
		  const char *filename)
{
  int  loc_single_file;
  char pre[3];
  char simname[512];
  loc_single_file = mcsingle_file; mcsingle_file = 1;
  
  if (!strstr(mcformat.Name, "McStas")) strcpy(pre,""); else strcpy(pre,"# ");
  
  mcfile_header(f, mcformat, "header", pre, mcinstrument_name, "mcstas");
  mcinfo_instrument(f, mcformat, pre, mcinstrument_name);
  if (mcdirname) sprintf(simname, "%s%s%s", mcdirname, MC_PATHSEP_S, mcsiminfo_name); else sprintf(simname, "%s%s%s", ".", MC_PATHSEP_S, mcsiminfo_name);

  mcfile_datablock(f, mcformat, 
    pre, parent, "data",
    NULL,NULL,NULL, m, n, p,
    xlabel, ylabel, zlabel, title,
    xvar, yvar, zvar, x1,  x2,  y1,  y2,  z1,  z2, 
    filename, 0);
  
  mcsingle_file = loc_single_file;
  mcfile_header(f, mcformat, "footer", pre, mcinstrument_name, "mcstas");
}


double mcdetector_out_0D(const char *t, double p0, double p1, double p2, char *c)
{
  char pre[20];
  
  strcpy(pre, "");

  return(mcdetector_out_012D(mcformat, 
    pre, c, t,
    1, 1, 1,
    "I", "", "", 
    "I", "", "", 
    0, 0, 0, 0, 0, 0, NULL,
    &p0, &p1, &p2));
}

double mcdetector_out_1D(const char *t, char *xl, char *yl,
                  char *xvar, double x1, double x2, int n,
                  double *p0, double *p1, double *p2, char *f, char *c)
{
  char pre[20];
  
  strcpy(pre, "");
  return(mcdetector_out_012D(mcformat, 
    pre, c, t,
    n, 1, 1,
    xl, yl, "Intensity", 
    xvar, "(I,I_err)", "I", 
    x1, x2, x1, x2, 0, 0, f,
    p0, p1, p2));
}

double mcdetector_out_2D(const char *t, char *xl, char *yl,
                  double x1, double x2, double y1, double y2, int m,
                  int n, double *p0, double *p1, double *p2, char *f, char *c)
{
  char xvar[3];
  char yvar[3];
  char pre[20];
  
  strcpy(pre, ""); strcpy(xvar, "x "); strcpy(yvar, "y ");
  if (xl && strlen(xl)) strncpy(xvar, xl, 2);
  if (yl && strlen(yl)) strncpy(yvar, yl, 2);
  
  return(mcdetector_out_012D(mcformat, 
    pre, c, t,
    m, n, 1,
    xl, yl, "Intensity", 
    xvar, yvar, "I", 
    x1, x2, y1, y2, 0, 0, f,
    p0, p1, p2));
}

double mcdetector_out_3D(const char *t, char *xl, char *yl, char *zl,
      char *xvar, char *yvar, char *zvar,
                  double x1, double x2, double y1, double y2, double z1, double z2, int m,
                  int n, int p, double *p0, double *p1, double *p2, char *f, char *c)
{
  char pre[20];
  
  strcpy(pre, "");
  return(mcdetector_out_012D(mcformat, 
    pre, c, t,
    m, n, p,
    xl, yl, zl, 
    xvar, yvar, zvar, 
    x1, x2, y1, y2, z1, z2, f,
    p0, p1, p2));
}
 
/* end of file i/o functions */



void mcuse_format(char *format)
{
  int i,j;
  int i_format=-1;
  char *tmp;
  char low_format[256];
  
  /* get the format to lower case */
  if (!format) return;
  strcpy(low_format, format);
  for (i=0; i<strlen(low_format); i++) low_format[i]=tolower(format[i]);
  if (!strcmp(low_format, "pgplot")) strcpy(low_format, "mcstas");
  tmp = (char *)malloc(256);
  if(!tmp) exit(fprintf(stderr, "Error: insufficient memory (mcuse_format)\n"));
  
  /* look for a specific format in mcformats.Name table */
  for (i=0; i < mcNUMFORMATS; i++)
  {
    strcpy(tmp, mcformats[i].Name); 
    for (j=0; j<strlen(tmp); j++) tmp[j] = tolower(tmp[j]);
    if (strstr(low_format, tmp)) i_format = i;
  }
  if (i_format < 0)
  {
    i_format = 0; /* default format is #0 McStas */
    fprintf(stderr, "Warning: unknown output format '%s'. Using default (%s).\n", format, mcformats[i_format].Name);
  }

  mcformat = mcformats[i_format];
  strcpy(tmp, mcformat.Name); 
  mcformat.Name = tmp;
  if (strstr(format,"binary"))
  {
    if (strstr(format,"double")) strcat(mcformat.Name," binary double data");
    else if (strstr(format,"NeXus")) strcat(mcformat.Name," binary NeXus data");
    else strcat(mcformat.Name," binary float data");
    mcascii_only = 1;
  }
} /* mcuse_format */


void mcuse_dir(char *dir)
{
#ifdef MC_PORTABLE
  fprintf(stderr, "Error: "
          "Directory output cannot be used with portable simulation.\n");
  exit(1);
#else  /* !MC_PORTABLE */
//   if(mkdir(dir, 0777))
//   {
//     //fprintf(stderr, "Error: unable to create directory '%s'.\n", dir);
//     //fprintf(stderr, "(Maybe the directory already exists?)\n");
//     //exit(1);
//   }
  mcdirname = dir;
#endif /* !MC_PORTABLE */
}


// version
// $Id$

// Generated automatically by CxxMill on Wed Jun 28 09:56:22 2006

// End of file 

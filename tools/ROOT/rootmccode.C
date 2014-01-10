/* 
Macro to produce ROOT histograms from McStas simulations (.dat or .sim file).
Authors: Erik B Knudsen and Esben Klinkby. Partly based on macro inherited from Peter Christiansen.

This to be considered experimental. 
Testing has so far been mimimal - plotting works with ROOT v. 5.34/14 on linux.

Milage reports are welcome (good or bad) to erkn_AT_fysik.dtu.dk
*/

#ifndef ROOT_TH1
#include "TH1.h"
#endif

#ifndef ROOT_TH2
#include "TH2.h"
#endif

#ifndef ROOT_TGraphErrors
#include "TGraphErrors.h"
#endif

#ifndef __IOSTREAM__
#include <iostream>
#endif

#ifndef __FSTREAM__
#include <fstream>
#endif

#include <string>
#include <list>
#include <sys/stat.h>

#include "TMap.h"
#include "TString.h"
#include "TObjString.h"
#include "TObject.h"


using std::cout;
using std::endl;

class mccoderun {
  bool compile,translate;
  string remaining;
  unsigned long long count;
  int seed;

  TMap *options = new TMap;
  
  public:
  bool check_compile();
  bool check_translate();
  int run();
  mccoderun(){
    compile=0;
    translate=0;
    ncount=1000000;
  }

  /*constructors - first one with parser*/
  mccoderun(string commandline=""){
    /*this code exctracts the running options*/
    istringstream iss(commandline);
    string token;
    while (iss>>token){
      int valid=0;
      string argument;
      if (token[0]=='-'){
        /*it's an option*/
        if (token[1]!='-'){
          /*short option*/
          switch(token[1]){
            case 'c':
              valid=1;
              compile=1;translate=1;break;
            case 'n':
              valid=1;
              if(token[2]){
                argument=token.substr(2);
              }else{
                iss>>argument;
              }
              ncount=strtol(argument.c_str(),NULL,10);
              break;
            case 's':
              valid=1;
              if(token[2]){
                argument=token.substr(2);
              }else{
                iss>>argument;
              }
              seed=strtol(argument.c_str(),NULL,10);
              break;
            default:
              break;
          }

        }
      }
      /*this was not an option*/
      if (!valid) remaining.append(token+" ");

    }

    /*remaining now has an extra spacehe end*/
    remaining=remaining.substr(0,remaining.length()-1);

  }

};

mccoderun(string instrument="",int help=0, int mpi=0, int fc=0, int ft=0, int trace=0, string instrument="", string outdir="", unsigned long long n=1000000, ){
               
}
       

mccoderun(string commandline){
  

}


class mccodescan {

};

int mcplot(const Char_t *infilename, bool verbose=0){
  /*generate a list of components(monitors) found in the mcstas.sim file*/
  
  TString *files[256];
  TString full(infilename);
  TString path = full(0,full.Last('/')+1);
  TString basename = full(full.Last('/')+1,full.Length());
  Int_t nplots=0;
  char ifn[512]="";
  
  if (basename=="")basename="mcstas.sim";
  if (basename == "mcstas.sim"){
    std::string line;
    strcat(ifn,path.Data());
    strcat(ifn,basename.Data());
    ifstream infile(ifn);
    if(!infile.good()){
      cout << "Cannot open file: " << ifn << endl;
      return 0;
    }
    while (std::getline(infile, line, '\n')){
      TString tline(line);
      //cout << line <<endl;
      if (tline.Contains("filename")){
        int i=line.find(':');
        TString t=tline(i+1,tline.Length());
        files[nplots] = new TString(path + t.Remove(TString::kBoth,' '));
        /*remove whitespace*/
        files[nplots]->Remove(TString::kBoth,' ');
        nplots++;
        files[nplots]=0;
      }
    }
  }else {
    /*only a single file wanted*/ 
    if(verbose) cout <<"opening single plot file"<<endl;
    ifstream infile(infilename);
    if(!infile.good()){
      cout << "Cannot open file: " << infilename << endl;
      return 0;
    }
    files[0]=new TString(infilename);
    files[1]=0;
    cout <<files[0]->Data()<<endl;
    nplots=1;
  }
  infile.close(); 


  int j;
  Int_t nx=1;
  Int_t ny=1;
  while (nx*ny<nplots){
    if (nx<=ny) nx++;
    else ny++;
  }
  TCanvas *c1 = new TCanvas();
  /*iterate through map and plot each datafile in pads on a canvas*/
  c1->Divide(nx,ny);
  TPad *p[256];
  TObject *plots[256];
  for (int i=0; i<nplots; i++){
    //p[i]=(TPad *)(c1->GetPrimitive("c1_"+i));
    c1->cd(i+1);
    char *s=files[i]->Data();
    if(verbose) cout <<i<<"Plotting: "<<s<<endl;
    //if( 
        plots[i]=mccode_plot_hist(s,"",1.0,1.0,verbose=verbose);
        //){
      /*so it worked*/
      plots[i]->Draw();   
    //}
  }
  //c1->Update()
  return 0;
}

/*to add canvas interactivity:
 * add and event handler function to subcanvas that reacts
 * to button press. This should then
 * a) store the subcanvas geometry
 * b) remove all other subcanvases from the parent
 * c) resize the subcanvas to full size
 * ... and then the other way around again when repressed.
 */


void mcsingle(const Char_t *filename, bool verbose=0)
{
  /*if mcstas.sim file - scan it
   * else setup a simple map with only one item in it*/
  
  
  /*set up a canvas*/
  //TCanvas c1 = new TCanvas();
  TMap *tm=scan_mccode_sim_file("gen_data/mcstas.sim",verbose=1);
  TIter *ti(tm);
  
  while (TPair *tp = (TPair *) ti()){
    cout << tp->Key();
  }
  /*figure out how many plots we need*/
   
return;

}

TObject *mccode_plot_hist(const Char_t *filename, const Char_t *histname, Float_t scaleX=1.0,
		 Float_t scaleY=1.0,
		 bool verbose=0){
    
    ifstream infile(filename);
    if(!infile.good()){
      cout << "Cannot open file: " << filename << endl;
      return 0;
    }
    
    std::string line;
    /*read and parse the file header*/
    TMap *header = new TMap;
    
    std::getline(infile, line, '\n');
    while(line[0]=='#') {
      TString tline(line.erase(0,1));
      /*Comment line - store it in the header*/
      int i=line.find(':');
      int j=line.find_first_not_of(' ',i+1);
      TString *t0 = new TString(tline(0,i));
      TString *t1 = new TString(tline(j,tline.Length()-1));
      t0->Remove(TString::kBoth,' ');
      t1->Remove(TString::kBoth,' ');
      header->Add(new TObjString(*t0),new TObjString(*t1));
      std::getline(infile, line, '\n');
    } 
    infile.close();

    TString str;
    str = ( (TObjString *) (header->GetValue("type")))->GetString();
    /*now we know what kind it is*/
    if (str.Contains("array_1d")){
      return mccode_plot_1dhist(filename, histname, scaleX, scaleY, verbose=verbose);
    }else if(str.Contains("array_2d")){
      return mccode_plot_2dhist(filename, histname, scaleX, scaleY, verbose=verbose);
    }
}

TH1F *mccode_plot_1dhist(const Char_t *filename, const Char_t *histname, Float_t scaleX=1.0,
		 Float_t scaleY=1.0,
		 bool verbose=0){
    // Read the content of a PGPLOT file into a histogram
    
    TH1F* hist = 0;

    Int_t nBins = 0;
    Float_t xmin = 0, xmax = 0;
    Float_t x, y, ey, nsim;

    ifstream infile(filename);
    if(!infile.good()){
      cout << "Cannot open file: " << filename << endl;
      return 0;
    }
    
    std::string line;
    /*read and parse the file header*/
    TMap *header = new TMap;
    
    std::getline(infile, line, '\n');
    while(line[0]=='#') {
      TString tline(line.erase(0,1));
      /*Comment line - store it in the header*/
      int i=line.find(':');
      int j=line.find_first_not_of(' ',i+1);
      TString *t0 = new TString(tline(0,i));
      TString *t1 = new TString(tline(j,tline.Length()-1));
      t0->Remove(TString::kBoth,' ');
      t1->Remove(TString::kBoth,' ');
      if (verbose) cout << "# " << "key: " << *t0 << " value: " << *t1 <<endl;
      header->Add(new TObjString(*t0),new TObjString(*t1));
      std::getline(infile, line, '\n');
    } 
      
    /*extract some important values from the header - */
    std::string str;
    str = ( (TObjString *) (header->GetValue("type")))->GetString();
    sscanf (str.c_str(),"array_1d(%d)",&nBins);
    str = ( (TObjString *) (header->GetValue("xlimits")))->GetString();
    sscanf (str.c_str(),"%f %f",&xmin,&xmax);
    TString xlabel=( (TObjString *) (header->GetValue("xlabel")))->GetString(); 
    TString ylabel=( (TObjString *) (header->GetValue("ylabel")))->GetString(); 
    
    /*build title of detector name and statistics etc>*/
    TString compname=( (TObjString *) (header->GetValue("component")))->GetString();
    TString outfilename=( (TObjString *) (header->GetValue("filename")))->GetString();
    TString statistics=( (TObjString *) (header->GetValue("statistics")))->GetString();

    /*Now read the data block 1D*/
    if(hist==0)
      if (histname!=""){
      hist = new TH1F(histname, 0, nBins, scaleX*xmin, scaleX*xmax);
      }else{
        hist = new TH1F(filename, 0, nBins, scaleX*xmin, scaleX*xmax);
      }   
    hist->Sumw2();
    
    while(std::getline(infile, line, '\n')){
      Int_t res = sscanf(line.c_str(), "%f %f %f %f", &x, &y, &ey, &nsim);
	
      if (res < 2) {
      // not a data line
        continue;
      }
      x *= scaleX;
	
      Int_t bin = hist->GetXaxis()->FindBin(x);
      hist->SetBinContent(bin, y);
      hist->SetBinError(bin, ey);
    }
  
  /*set a few plot details*/    
  
    hist->GetYaxis()->SetTitle(ylabel);
    hist->GetXaxis()->SetTitle(xlabel);
  
    hist->SetLineWidth(1);
    hist->SetStats(0);
    hist->SetTitle(compname +" ["+outfilename+"] " + statistics.ReplaceAll(";",""));
    hist->SetOption("E1"); 
  
    return hist;
} 

TH2F *mccode_plot_2dhist(const Char_t *filename, const Char_t *histname, Float_t scaleX=1.0,
		 Float_t scaleY=1.0,
		 bool verbose=0){
    // Read the content of a PGPLOT file into a histogram
    
    TH2F* hist = 0;
     
    Int_t nBinsX = 0, nBinsY;
    Float_t xmin = 0, xmax = 0;
    Float_t ymin = 0, ymax = 0;
    Float_t x, y, ex, ey, nsim;

    ifstream infile(filename);
    if(!infile.good()){
      cout << "Cannot open file: " << filename << endl;
      return 0;
    }
    
    std::string line;
    /*read and parse the file header*/
    TMap *header = new TMap;
    
    std::getline(infile, line, '\n');
    while(line[0]=='#') {
      TString tline(line.erase(0,1));
      /*Comment line - store it in the header*/
      int i=line.find(':');
      int j=line.find_first_not_of(' ',i+1);
      TString *t0 = new TString(tline(0,i));
      TString *t1 = new TString(tline(j,tline.Length()-1));
      t0->Remove(TString::kBoth,' ');
      t1->Remove(TString::kBoth,' ');
      if (verbose) cout << "# " << "key: " << *t0 << " value: " << *t1 <<endl;
      header->Add(new TObjString(*t0),new TObjString(*t1));
      if (!t0->BeginsWith("Data")) std::getline(infile, line, '\n');
    } 
      
    /*extract some important values from the header - */
    std::string str;
    str = ( (TObjString *) (header->GetValue("type")))->GetString();
    sscanf (str.c_str(),"array_2d(%d,%d)",&nBinsX,&nBinsY);
    str = ( (TObjString *) (header->GetValue("xylimits")))->GetString();
    sscanf (str.c_str(),"%f %f %f %f",&xmin,&xmax,&ymin,&ymax);
    str = ( (TObjString *) (header->GetValue("variables")))->GetString();
    TString xlabel=( (TObjString *) (header->GetValue("xlabel")))->GetString(); 
    TString ylabel=( (TObjString *) (header->GetValue("ylabel")))->GetString(); 
    TString zlabel=( (TObjString *) (header->GetValue("zlabel")))->GetString(); 
  
    /*build title of detector name and statistics etc>*/
    TString compname=( (TObjString *) (header->GetValue("component")))->GetString();
    TString outfilename=( (TObjString *) (header->GetValue("filename")))->GetString();
    TString statistics=( (TObjString *) (header->GetValue("statistics")))->GetString();

    xmin*=scaleX; xmax*=scaleX;
    ymin*=scaleY; ymax*=scaleY;
	
    hist = new TH2F(histname, 0, nBinsX, xmin, xmax,
			nBinsY, ymin, ymax);
	
    const Float_t dx = (xmax-xmin)/Float_t(nBinsX);
    const Float_t dy = (ymax-ymin)/Float_t(nBinsY);
    Int_t *binx=malloc(nBinsX*sizeof(Int_t));
    Int_t *biny=malloc(nBinsY*sizeof(Int_t));
    Double_t val;

    for(Int_t i = 0; i < nBinsX; i++){
       binx[i] = hist->GetXaxis()->FindBin(xmin+(Float_t(i)+0.5)*dx);
       if(verbose)cout << "bin " <<i << "@ " << binx[i] << "approx " << xmin+(Float_t(i)+0.5)*dx <<endl;
    }
    for(Int_t i = 0; i < nBinsY; i++)
       biny[i] = hist->GetYaxis()->FindBin(ymin+(Float_t(i)+0.5)*dy);

    //done reading header, starting to read the data
    for(Int_t j = 1; j <= nBinsY; j++) {
      for(Int_t i = 1; i <= nBinsX; i++) {
        Int_t bin=hist->GetBin(i,j);
        infile >> val;
	hist->SetBinContent(bin, Float_t(val));
      }
    }	

    hist->GetXaxis()->SetTitle(xlabel);
    hist->GetYaxis()->SetTitle(ylabel);
    hist->GetZaxis()->SetTitle(zlabel);

    hist->SetStats(0);
    hist->SetTitle(compname +" ["+outfilename+"] " + statistics.ReplaceAll(";",""));
    //hist->GetZaxis()->SetTitleOffset(1.4);
    hist->SetOption("colz"); 
    return hist;
}

void mcstas2root(const Char_t* filename, 
		 const Char_t* histname="myhistogram.root",
		 Float_t scaleX=1.0,
		 Float_t scaleY=1.0,
		 bool verbose=0)
{ 
  TFile *file = new TFile(histname,"RECREATE");
  //  std::cout<<format<<std::endl;
  string strhash = "";
  string strdummy = "";
  char strfilename[80]='\0';

  //  ! ".L /home/klinkby/rootalias.C;"
  //  ! "setpawstyle();"
  // Read the content of a PGPLOT file into a histogram
  

  //  Int_t nBins = 0;
  //  Float_t xmin = 0, xmax = 0;
  //  Float_t x, y, ey, nsim;

  
  ifstream infile(filename);
  if(!infile.good()){
    cout << "Cannot open file: " << filename << endl;
    return 0;
  }
  
  std::string line;
  bool plot2D = false;
  while(std::getline(infile, line, '\n')) {
    if (verbose==true)  cout<<line<<endl;

    TString helpString(line);
    if (helpString.Contains("filename")) std::istringstream(line,ios_base::in) >> strhash >> strdummy >> strfilename;
    if (helpString.Contains("zvar")) plot2D = true;
  }
  
  

  if (verbose==true) {
    if (plot2D == false) std::cout<<" Plot 1D found "<<std::endl;
    else if (plot2D == true)  std::cout<<" Plot 2D found "<<std::endl;
  }


    string xlab="";
    string ylab="";
    
    string str1 = "";
    char str2[80]='\0';
    char str3[80]='\0';
    char str4[80]='\0';
    char str5[80]='\0';
    char str_xlab[80] = '\0';
    char str_ylab[80] = '\0';

		  

  // Case 1D
  if (!plot2D) {
    // Read the content of a PGPLOT file into a histogram
    
    TH1F* hist = 0;
  
    Int_t nBins = 0;
    Float_t xmin = 0, xmax = 0;
    Float_t x, y, ey, nsim;
  
    ifstream infile(filename);
    if(!infile.good()){
      //     MakeZombie();
      cout << "Cannot open file: " << filename << endl;
      return 0;
    }
    
    std::string line;
    while(std::getline(infile, line, '\n')) {
      
      TString helpString(line);
      
      if(helpString.Contains("#")) {
	
	if(helpString.Contains("type")) {
	  
        	  sscanf (line.c_str(),"# type: array_1d(%d)",&nBins);
	} else if(helpString.Contains("xlimits")){
	  
	  sscanf (line.c_str(),"# xlimits: %f %f", &xmin, &xmax);	
	}
	
	else if(helpString.Contains("xlabel")) {
	  std::istringstream(line,ios_base::in) >> strhash >> str1>> str2 >>str3;
	  strcat(str_xlab,str2);
	  strcat(str_xlab,str3);
      }
	else if(helpString.Contains("ylabel")) {
	  strhash = "";
	  str1 = "";
	  std::istringstream(line,ios_base::in) >> strhash >> str1>> str4 >>str5;
	  strcat(str_ylab,str4);
	  strcat(str_ylab,str5);
      }
	
	//Done reading header, starting reading the data		
      } else {
	Float_t offset = 27.;
	if(hist==0)
	  hist = new TH1F(histname, 0, nBins, scaleX*xmin+offset, scaleX*xmax+offset);
	
	Int_t res = sscanf(line.c_str(), "%f %f %f %f", &x, &y, &ey, &nsim);
	
	if (res < 2) {
	  // not a data line
	  continue;
	}
	
	x *= scaleX;
	x += offset;
	
	Int_t bin = hist->GetXaxis()->FindBin(x);
	hist->SetBinContent(bin, y);
	hist->SetBinError(bin, ey);
      }
    }
    
    hist->GetYaxis()->SetTitle(str_ylab);
    hist->GetXaxis()->SetTitle(str_xlab);

    hist->Draw();
    hist->SetLineWidth(2);
    hist->SetStats(0);
    hist->Write();
    

    //  char imagefile[80]='\0';
    //  char form2[80]="pdf";
    //  strcat(imagefile,strfilename);
    //  strcat(imagefile,form2);
    //  hist->Print(imagefile);
    c1->SetLogy();

    c1->Print("1Dplot.pdf");
    c1->Print("1Dplot.eps");
    
    return;


    
    //Case 2D
  } else {
    
    // Read the content of a PGPLOT file into a histogram
    
    TH2F* hist = 0;
  
    int  nBinsX = 0, nBinsY = 0;
    Float_t xmin = 0, xmax = 0, ymin = 0, ymax = 0;
    
    
    ifstream infile(filename);
    if(!infile.good()){
      //     MakeZombie();
      cout << "Cannot open file: " << filename << endl;
      return 0;
    }
    
    std::string line;
    double val = 0;
    

    while(std::getline(infile, line, '\n')) {
      if (verbose==true)  cout<<line<<endl;
    
      TString helpString(line);
      
      if(helpString.Contains("#")) {
	
	if(helpString.Contains("type")) {
	  sscanf (line.c_str(),"# type: array_2d(%d, %d)",&nBinsX, &nBinsY);

      } else if(helpString.Contains("xylimits")){
	sscanf (line.c_str(),"# xylimits: %f %f %f %f", 
		&xmin, &xmax, &ymin, &ymax);	

      } else if(helpString.Contains("variables")){
	xmin*=scaleX;
	xmax*=scaleX;
	ymin*=scaleY;
	ymax*=scaleY;
	//	cout << "X: " << nBinsX << ", " << xmin << ", " << xmax << endl;
	//	cout << "Y: " << nBinsY << ", " << ymin << ", " << ymax << endl;
	
	hist = new TH2F(histname, 0, nBinsX, xmin, xmax,
			nBinsY, ymin, ymax);
	
	const int *nx = nBinsX;
	const int *ny = nBinsY;
	const Float_t dx = (xmax-xmin)/Float_t(nx);
	const Float_t dy = (ymax-ymin)/Float_t(ny);
	int binx[&nx];
	int biny[&ny];

	for(Int_t i = 0; i < nBinsX; i++) 
	  binx[i] = hist->GetXaxis()->FindBin(xmin+(Float_t(i)+0.5)*dx);
	
	for(Int_t i = 0; i < nBinsY; i++)
	  biny[i] = hist->GetYaxis()->FindBin(ymin+(Float_t(i)+0.5)*dy);
      }

      if(helpString.Contains("xlabel")) {
        std::istringstream(line,ios_base::in) >> strhash >> str1>> str2 >>str3;
	strcat(str_xlab,str2);
	strcat(str_xlab,str3);
      }

      if(helpString.Contains("ylabel")) {
	strhash = "";
	str1 = "";
        std::istringstream(line,ios_base::in) >> strhash >> str1>> str4 >>str5;
	strcat(str_ylab,str4);
	strcat(str_ylab,str5);
      }

    }
    //done reading header, starting to read the data
      else {

	for(Int_t j = 0; j < nBinsY; j++) {
	  for(Int_t i = 0; i < nBinsX; i++) {
	    infile >> val;
	    hist->SetCellContent(binx[i], biny[j], Float_t(val));
	  }
	}	
    }
  }

    // hist->GetYaxis()->SetTitle(str_ylab);
    //  hist->GetXaxis()->SetTitle(str_xlab);
    //    TCanvas *c1 = new TCanvas("c1","multipads",1200,700);
    hist->GetXaxis()->SetTitle("y [m]");
    hist->GetYaxis()->SetTitle("m");
    hist->GetZaxis()->SetTitle("Signal pr bin");

    hist->Draw("colz");
    hist->SetStats(0);
    hist->Write();
    hist->GetZaxis()->SetTitleOffset(1.4);
    //    TCanvas c1=new TCanvas("c1", "First canvas", 400, 300); // problems here
    //    c1->SetLeftMargin(0.15);
    c1->SetRightMargin(0.15);

    c1->Print("2Dplot.pdf");
    c1->Print("2Dplot.eps");
  
  //  char imagefile[80] = '\0';
  //  strcat(imagefile,filename);
  //  strcat(imagefile,format);

  //  hist->Print(imagefile);

  return;
}

}
//___________________________________________________________________________
TGraphErrors* ReadPGPLOTGraph(const char *filename, Float_t scaleX)
{
  // Read the content of a PGPLOT file into a graph
  TGraphErrors* graph = new TGraphErrors(300);
  
  //  CtorAllocate();
  Double_t x, y, ey, nsim;
  
  ifstream infile(filename);
  if(!infile.good()){
    //     MakeZombie();
    cout << "Cannot open file: " << filename << endl;
    delete graph;
    return 0;
  }
  
  Int_t np = 0;
  
  std::string line;
  while(std::getline(infile, line, '\n')) {
    
    Int_t res = sscanf(line.c_str(), "%lg %lg %lg %lg", &x, &y, &ey, &nsim);
    
    if (res < 2) {
      // not a data line
      continue;
    }

    graph->SetPoint(np, scaleX*x, y);
    graph->SetPointError(np, 0, ey);
    np++;
  }
  graph->Set(np);

  return graph;
}

//___________________________________________________________________________
Double_t FindGraphMax(TGraph* graph)
{

  Int_t nExp = graph->GetN();
  Float_t max = 0;

  for(Int_t i = 0; i < nExp; i++) {
    
    if(graph->GetY()[i]>max)
      max = graph->GetY()[i];
  } 
  
  return max;
}

//___________________________________________________________________________
Int_t FindGraphMaxBin(TGraph* graph)
{

  Int_t   nExp = graph->GetN();
  Float_t max = 0;
  Int_t   bin = -1;

  for(Int_t i = 0; i < nExp; i++) {
    
    if(graph->GetY()[i]>max) {
      max = graph->GetY()[i];
      bin = i;
    }
  } 
  
  return bin;
}       

//______________________________________________________________________________
int plot(int d) {
  return 2*d;
}


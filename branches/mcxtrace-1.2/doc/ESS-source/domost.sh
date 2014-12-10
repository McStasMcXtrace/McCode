latex mcstas_ess_source.tex  
bibtex mcstas_ess_source
latex mcstas_ess_source.tex
latex mcstas_ess_source.tex && dvips mcstas_ess_source.dvi -o mcstas_ess_source.ps
gv mcstas_ess_source.ps
echo 'gv mcstas_ess_source.ps'

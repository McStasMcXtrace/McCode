latex mcstas_ess_source.tex  
bibtex mcstas_ess_source
latex mcstas_ess_source.tex
latex mcstas_ess_source.tex && dvips mcstas_ess_source.dvi -o mcstas_ess_source.ps
ps2pdf  mcstas_ess_source.ps
echo 'acroread mcstas_ess_source.pdf'
echo 'gv mcstas_ess_source.ps'

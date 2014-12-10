#!/bin/sh

NAME="$1"

echo "Building ${NAME}"

latexmk -silent -bibtex -pdf ${NAME}.tex
#for prog in pdflatex bibtex makeindex pdflatex pdflatex; do
#    echo "> ${prog}";
#    ${prog} "${NAME}" > /dev/null || exit 1;
#done

#echo ""


#DVIPDF="`command -v dvipdf`"

#DVIPS="`command -v dvips`"
#PSPDF="`command -v ps2pdf`"

#if [ -x "${DVIPDF}" ]; then
#    echo "Generate PDF file using dvipdf (better quality than ps2pdf)";
#    ${DVIPDF} ${NAME}.dvi ${NAME}.pdf;
#else
#    echo "Generate PDF file using dvips and ps2pdf";
#    dvips -o ${NAME}.ps ${NAME}.dvi;
#    ps2pdf ${NAME}.ps;
#fi

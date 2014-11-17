#!/bin/sh

NAME="$1"

echo "Building ${NAME}"

latexmk -silent -bibtex -xelatex ${NAME}.tex

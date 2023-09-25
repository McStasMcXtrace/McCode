#!/bin/sh
#
# Script for bootstrapping a Mambaforge/miniconda3 environment in a McStas/McXtrace installataion
# on Unix

MCCODEINSTALL=${1}

# Download the installer
cd `dirname $0`
curl -L -O "https://github.com/conda-forge/miniforge/releases/latest/download/Mambaforge-$(uname)-$(uname -m).sh"
chmod a+x Mambaforge-$(uname)-$(uname -m).sh
./Mambaforge-$(uname)-$(uname -m).sh -b -p $MCCODEINSTALL/miniconda3/
source $MCCODEINSTALL/miniconda3/bin/activate
mamba env update -n base -f environment.yml

# Adapt McStasScript to bundle version
MCSCONFIG=`find ${MCCODEINSTALL}/miniconda3/lib -type d -name mcstasscript`
MCSCONFIG=${MCSCONFIG}/configuration.yaml
echo McStasScript config is found in ${MCSCONFIG}

echo "other:" > ${MCSCONFIG}
echo "  characters_per_line: 85" >> ${MCSCONFIG}
echo "paths:" >> ${MCSCONFIG}
echo "  mcrun_path: ${MCCODEINSTALL}/bin" >> ${MCSCONFIG}
echo "  mcstas_path: ${MCCODEINSTALL}" >> ${MCSCONFIG}
echo  >> ${MCSCONFIG} 

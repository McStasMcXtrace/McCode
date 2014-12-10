mcrun ESS_Brilliance_2014.instr -n1e7 frac=1 flat=0 srcdef=2014 -d 2014_cold_mcnpxlike
mcrun ESS_Brilliance_2014.instr -n1e7 frac=1 flat=1 srcdef=2014 -d 2014_cold_flat
mcrun ESS_Brilliance_2014.instr -n1e7 frac=1 flat=0 srcdef=TDR -d TDR_cold_mcnpxlike
mcrun ESS_Brilliance_2014.instr -n1e7 frac=1 flat=1 srcdef=TDR -d TDR_cold_flat
mcrun ESS_Brilliance_2014.instr -n1e7 frac=1 flat=0 srcdef=2001 -d 2001_cold_mcnpxlike
mcrun ESS_Brilliance_2014.instr -n1e7 frac=1 flat=1 srcdef=2001 -d 2001_cold_flat

mcrun ESS_Brilliance_2014.instr -n1e7 frac=0 flat=0 srcdef=2014 lambdamax=4 -d 2014_thermal_mcnpxlike
mcrun ESS_Brilliance_2014.instr -n1e7 frac=0 flat=1 srcdef=2014 lambdamax=4 -d 2014_thermal_flat
mcrun ESS_Brilliance_2014.instr -n1e7 frac=0 flat=0 srcdef=TDR lambdamax=4 -d TDR_thermal_mcnpxlike
mcrun ESS_Brilliance_2014.instr -n1e7 frac=0 flat=1 srcdef=TDR lambdamax=4 -d TDR_thermal_flat
mcrun ESS_Brilliance_2014.instr -n1e7 frac=0 flat=0 srcdef=2001 lambdamax=4 -d 2001_thermal_mcnpxlike
mcrun ESS_Brilliance_2014.instr -n1e7 frac=0 flat=1 srcdef=2001 lambdamax=4 -d 2001_thermal_flat

mcrun ESS_Brilliance_2014.instr -n1e7 frac=0.5 flat=0 srcdef=2014 -d 2014_bisp_mcnpxlike
mcrun ESS_Brilliance_2014.instr -n1e7 frac=0.5 flat=0 srcdef=TDR -d TDR_bisp_mcnpxlike
mcrun ESS_Brilliance_2014.instr -n1e7 frac=0.5 flat=0 srcdef=2001 -d 2001_bisp_mcnpxlike

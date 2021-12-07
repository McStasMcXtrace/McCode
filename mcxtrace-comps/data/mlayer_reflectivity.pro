; IDL procedures and function ot help generate reflectivity data for use with McXtrace.
; These procedures rely on calling IMD (available from http://www.rxollc.com/idl/index.html) for the actual calculation.
; At the top of this file are some examples on how to call the scripts.

; An example run that generates a 10 AA layer of SiC on top of 50 A Ir
; on a SiO2-substrate, at 100 q-samples in the range q\in[1e-6,5] AA^-1.
; Data will be written to the file 'data_output'
; Please copy and edit to generate datafiles.
pro SiC_Ir_SiO2
  zdepths=[10,50]; AA
  materials=['SiC','Ir']
  qrange=[1e-6,100,5]; AA^-1
  qcompute, materials, zdepths, qrange, OUTFILE='data_output', SUBSTRATE='SiO2', COMMENT='10 AA SiC on 50 AA Ir. SiO2 substrate'
END

; Same as above, but sends data to the console. This ought to work for
; demo-idl as well.
pro SiC_Ir_SiO2_console
  zdepths=[10,50]; AA
  materials=['SiC','Ir']
  qrange=[1e-6,100,5]; AA^-1
  qcompute, materials, zdepths, qrange, SUBSTRATE='SiO2', COMMENT='10 AA SiC on 50 AA Ir. SiO2 substrate'
END

; Similar to above but generates a matrix of reflectity numbers as a
; function of energy and glacing angle
pro SiC_Ir_SiO2_eth
  zdepths=[10,50]; AA
  materials=['SiC','Ir']
  Erange=[1e-6,100,25]; keV
  theta_range=[1e-3,100,2]; deg.
  ethcompute, materials, zdepths, Erange, theta_range, OUTFILE='output_data_eth', SUBSTRATE='SiO2', COMMENT='10 AA SiC on 50 AA Ir. SiO2 substrate'
END


; Return an array of optical constants for 
; the materials in a multilayer on a glass (SiO2) substrate,
; inputs:
; arrLambda: [AA] array of wavelengths at which to compute
; layMat: [str]   array of strings corresponding to materials in the IMD database
; rho: [g/cm^3]   array of densities for the materials. Must be same length as layMat
; output:
; 2d-array of complex optical constants 
function mlayer_optical_constants, arrLambda, layMat, subMat 
  nLayer = n_elements(layMat); the number of layers on top of substrate
  oc_arr = dcomplexarr(nLayer+2,n_elements(arrLambda)) ; optical constants array, including ambient and substrate
  print, "optical constants matrix size (nL+2)*(nlambda): =(",nLayer,"+2)*",n_elements(arrLambda),"=",n_elements(oc_arr)
  
  ;add vacuum ambient
  oc_arr(0,*) = complex(1.0,0.) ; vacuum ambient
  
  ;loop over materials
  for i=0,nLayer-1 do begin
     oc_arr(i+1,*) = IMD_NK(layMat(i),arrLambda);
  end 
  ;add SIO2 substrate 
  oc_arr(nLayer+1,*) = IMD_NK(subMat,arrLambda) ; SiO2 substrate
  
  print, 'done layer def'
  return, oc_arr
END

; Return an array of optical constants based solely on density and f1,f2,
; for a multilayer on a glass (SiO2) substrate.
; input:
; arrLambda: [AA] array of wavelengths at which to compute
; layMat: [str]   array of strings corresponding to materials in the IMD database
; rho: [g/cm^3]   array of densities for the materials. Must be same length as layMat
; output:
; 2d-array of complex optical constants 
function layer_supergen_SiO2, arrLambda, layMat, rho
  nLayer = n_elements(layMat); the number of layers on top of substrate
  oc_arr = dcomplexarr(nLayer+2,n_elements(arrLambda)) ; optical constants array, including ambient and substrate
  print, "optical constants matrix size (nL+2)*(nlambda): =(",nLayer,"+2)*",n_elements(arrLambda),"=",n_elements(oc_arr)
  
  ;add vacuum ambient
  oc_arr(0,*) = complex(1.0,0.) ; vacuum ambient
  
  ;loop over materials
  for i=0,nLayer-1 do begin
     oc_arr(i+1,*) = IMD_F1F2TONK(rho(i),1,layMat(i),arrLambda);
  end 
  ;add SIO2 substrate 
  oc_arr(nLayer+1,*) = IMD_NK('SiO2',arrLambda) ; SiO2 substrate
  
  print, 'done layer def'
  return, oc_arr
END


; Compute reflectivities a a set of energies and angles and print
; them to file "outfile" in a format understandable by McXtrace
; input:
; outfile: [str]      output filename
; materials: [str]    array of strings that define the materials in IMD
; Zs: [AA]            array of layer thicknesses
; Es: [keV/#]         3-element array with specs for the energy range (min,numberOfSteps,max)
; ths: [deg./#]       3-element array with specs for the theta range of glancing angles (min,numberOfSteps,max)
; comment: [str]      additional comment to put in the header
; substrate: [str]    substrate material
pro ethcompute, OUTFILE=outfile, materials, Zs, COMMENT=comment, Es, ths, SUBSTRATE=substrate
  eN=Es[1]
  e_min=Es[0]
  e_max=Es[2]
  e_step=(e_max-e_min)/(eN-1)
  ;make an array of e's
  e = e_step*findgen(eN)+e_min

  print, Zs
  th_min=ths[0]
  th_max=ths[2]
  thN=ths[1]
  th_step=(th_max-th_min)/(thN-1)
  ;array of thetas
  th = th_step*findgen(thN)+th_min

  lambda=12.3984/e
  SIGMA=4.5
  
  if N_ELEMENTS(substrate) eq 0 THEN substrate='SiO2'

  NC=mlayer_optical_constants(lambda,materials, substrate)
  FRESNEL, (90-th), lambda, NC, Zs, SIGMA, RA=RA

  ; output data 
  if N_ELEMENTS(outfile) ne 0 THEN begin
     openw, 1, outfile, width=16*(n_elements(theta)+1)
     fid=1
  endif else begin
     fid=-1
     printf, fid, "--- DATA ---"
  endelse
  printf, fid, '#e_min='+strtrim(e_min,1)
  printf, fid, '#e_max='+strtrim(e_max,1)
  printf, fid, '#e_step='+strtrim(e_step,1)
  printf, fid, '#theta_min='+strtrim(th_min,1)
  printf, fid, '#theta_max='+strtrim(th_max,1)
  printf, fid, '#theta_step='+strtrim(th_step,1)
  printf, fid, comment
  
  for r=0,eN-1 do begin
     for c=0,thN-1 do begin
       printf, fid, RA[c,r], FORMAT='((F0)," ",$)'
    endfor
  printf,fid,''
  endfor

  if N_ELEMENTS(outfile) THEN close, fid
end

; Compute reflectivities for a multilayer at a set of wavevector tranfers, q, and print
; them to file "outfile" in a format understandable by McXtrace
; input:
; outfile: [str]      output filename
; materials: [str]    array of strings that define the materials in IMD
; Zs: [AA]            array of layer thicknesses
; qs: [deg./#]        3-element array with specs for the range of q (min,numberOfSteps,max)
; comment: [str]      additional comment to put in the header
pro qcompute, OUTFILE=outfile, materials, Zs, qs, COMMENT=comment, SUBSTRATE=substrate
  Emax=15
  Nq=qs[1]
  q_min=qs[0]
  q_max=qs[2];2*15*sin(3*!pi/180)
  q_step=(q_max-q_min)/Nq
  ;make an array of qs
  q = q_step*findgen(round((q_max-q_min)/q_step+1))+q_min
 
  SIGMA = 4.5 ;roughness a a single value applied to all layers

  ;dblarr(0);
  ; ============================================
  ; IMD
  ; ============================================
  ; ============================================
  print, "Thickness array length: ", n_elements(Zs)
  print, "Sigma array length: ", n_elements(SIGMA)

  if N_ELEMENTS(substrate) eq 0 THEN substrate='SiO2'

  ;some grazing incidence angle (we vary energy to vary q)
  ;should perhaps add something more clever here to check 
  ; for unreachable q or to avoid loss of resolution but as long as
  ; angles are small we should be fine.
  theta=1.5
  E2K=0.506773091264796
  
  ;compute an Energy,lambda combination that matches q
  k=q/(2*sin(theta*!pi/180))
  lambda=2*!pi/k
  ; define multilayer
  NC = mlayer_optical_constants(lambda, materials, substrate)
  FRESNEL, (90-theta), lambda, NC, Zs, SIGMA, RA=RA


  ; output data 
  if N_ELEMENTS(outfile) ne 0 THEN begin
     openw, 1, outfile, width=16*(n_elements(theta)+1)
     fid=1
  endif else begin
     fid=-1
     printf, fid, "--- DATA ---"
  endelse
  printf, fid, '#param=q'
  printf, fid, '#q_min='+strtrim(q_min,1)
  printf, fid, '#q_max='+strtrim(q_max,1)
  printf, fid, '#q_step='+strtrim(q_step,1)
  if N_ELEMENTS(comment) ne 0 THEN printf, fid, '#'+comment
  for i=0, n_elements(q)-1 do begin &$
    printf, fid, q(i),RA(i)
  endfor
  if N_ELEMENTS(outfile) THEN close, fid
end

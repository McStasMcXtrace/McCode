"" Vim syntax file for writing mcstas instruments
"" Author: Erik Knudsen erik.knudsen@risoe.dk
"" version 0.1 (may 2007) 

"" insert something like:
"" augroup mcstas_instr
""  autocmd BufRead,BufNewFile *.instr,*.comp setfiletype mcstas 
"" augroup END
""  in your .vimrc file (_vimrc for windows) and put this file in the 
""  .vim/syntax/ directory (or equivalent) and it should be loaded
""  when opening a *.instr (or *.comp) -file

" mcstas instruments are mostly c syntax so start with that
:runtime! syntax/c.vim

"keywords for mcstas statements
:syntax keyword instrStatement DECLARE DEFINE END FINALLY INITIALIZE MCDISPLAY SAVE SHARE
:syntax keyword instrStatement TRACE DEFINITION PARAMETERS POLARISATION SETTING STATE
:syntax keyword instrStatement OUTPUT INSTRUMENT include
:highlight link instrStatement Statement

"mcstas runtime keywords
:syntax keyword instrRuntime
	\ ABSORB 
	\ SCATTERED 
	\ MC_GETPAR 
	\ DETECTOR_OUT 
	\ DETECTOR_OUT_0D 
	\ DETECTOR_OUT_1D 
	\ DETECTOR_OUT_2D 
	\ DETECTOR_OUT_3D 
	\ NAME_CURRENT_COMP 
	\ INDEX_CURRENT_COMP 
	\ POS_A_CURRENT_COMP 
	\ POS_R_CURRENT_COMP 
	\ ROT_A_CURRENT_COMP 
	\ ROT_R_CURRENT_COMP 
	\ RAD2MIN 
	\ MIN2RAD 
	\ DEG2RAD 
	\ RAD2DEG 
	\ K2V 
	\ V2K 
	\ Q2V 
	\ V2Q 
	\ SE2V 
	\ VS2E 
	\ FWHM2RMS 
	\ RMS2FWHM 
	\ HBAR 
	\ MNEUTRON 
	\ PI 
	\ POS_A_COMP_INDEX 
	\ POS_R_COMP_INDEX 
	\ NAME_COMP 
	\ POS_A_COMP 
	\ POS_R_COMP 
	\ ROT_A_COMP 
	\ ROT_R_COMP 
	\ SCATTER 
	\ STORE_NEUTRON 
	\ RESTORE_NEUTRON 
	\ PROP_GRAV_DT 
	\ PROP_DT 
	\ PROP_Z0 
	\ PROP_X0 
	\ PROP_Y0 
	\ vec_prod 
	\ scalar_prod 
	\ NORM 
	\ rotate 
	\ rand01 
	\ randpm1 
	\ rand0max 
	\ randminmax 
	\ normal_vec 
	\ box_intersect 
	\ cylinder_intersect 
	\ sphere_intersect 
	\ randvec_target_circle 
	\ randvec_target_rect_angular 
	\ randvec_target_rect 
:highlight link instrRuntime Constant

"mcstas trace keywords -these should ideally only be highlighted when within
"the TRACE region or something
:syntax keyword instrTrace ABSOLUTE AT COMPONENT EXTEND GROUP PREVIOUS NEXT MYSELF RELATIVE ROTATED WHEN JUMP ITERATE SPLIT COPY
highlight link instrTrace Statement

"mcstas type keywords
:syntax keyword instrType auto char const double float int long register restrict short signed static unsigned void volatile _Imaginary _Complex _Bool
highlight link instrType Type

"mcstas attention keywords 
:syntax keyword instrATT FIXME TODO ### Identification Description Parameters Link
:highlight link instrATT Todo
"here the insertion menus could be put

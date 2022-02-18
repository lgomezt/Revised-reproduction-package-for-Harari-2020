version 13
cap restore
cap log close
clear all
set more off
set matsize 9000

*=================================================================================================*
** Set up
*=================================================================================================*

**********************************************************************************
************************************ Paths ***************************************
**********************************************************************************

global resultsfolder 	".\Out\"
global datafoldernew 	".\Data\"
global logfolder 		".\Log"

log using "${logfolder}\Table13_FARs.log", replace 

**********************************************************************************
************************************ Tablesout ***********************************
**********************************************************************************

* Table 13: Impact of FARs on city shape 
* Table13_FAR.xls
*
*=================================================================================================*
** Regressions
*=================================================================================================*

**********************************************************************************
********************************** Set up ****************************************
**********************************************************************************

use "$datafoldernew\CityShape_Main.dta", clear

foreach var of varlist * { 
	local newname = subinstr("`var'","disconnect","d",.) 
	ren `var' `newname'
}

local y d


**********************************************************************************
*************************** Generate interactions ********************************
**********************************************************************************

gen shape_instr_x_FAR = r1_relev_`y'_cls_km * FAR
gen area_instr_x_FAR = log_projected_pop * FAR

* Labels for regressions
label var area_instr_x_FAR "Log projected population x FAR"
label var shape_instr_x_FAR "Potential shape, km x FAR"
label var r1_relev_d_cls_km "Potential shape, km"

**********************************************************************************
******************************* OLS regressions **********************************
**********************************************************************************

local tosort log_projected_pop area_instr_x_FAR r1_relev_d_cls_km shape_instr_x_FAR

*** First stage for area with both instruments interacted
quie reg `y'_km  shape_instr_x_FAR area_instr_x_FAR r1_relev_`y'_cls_km log_projected_pop i.year i.id ,  cluster(id)
outreg2 using "$resultsfolder/Table13_FAR.xls" ,  nocons nor2  title(Table 13: Impact of FARs on city shape) alpha (0.01, 0.05 , 0.1 ,  0.15) symbol (***, **, *, +)  sortvar (`tosort') ctitle (OLS, "Shape, km" ) keep(shape_instr_x_FAR area_instr_x_FAR r1_relev_`y'_cls_km log_projected_pop) addtext(City FE, Y, Year FE, Y)   label(insert) replace

quie reg log_area_polyg_km shape_instr_x_FAR area_instr_x_FAR  r1_relev_`y'_cls_km log_projected_pop i.year i.id,  cluster(id)
outreg2 using "$resultsfolder/Table13_FAR.xls" ,  nocons nor2  alpha (0.01, 0.05 , 0.1 ,  0.15) symbol (***, **, *, +)  sortvar (`tosort') ctitle (OLS, "Log area, km2") keep(shape_instr_x_FAR area_instr_x_FAR r1_relev_`y'_cls_km log_projected_pop)  addtext(City FE, Y, Year FE, Y)   label(insert) append 


**********************************************************************************
**********************************************************************************
**********************************************************************************

log close 
* end


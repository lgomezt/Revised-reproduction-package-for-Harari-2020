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

log using "${logfolder}\Table8_PopulationLD_SingleIV.log", replace 

**********************************************************************************
************************************ Tablesout ***********************************
**********************************************************************************

* Table 8: First stage and impact of city shape on population, single instrument				
* Table8_SingleInstrument.xls

*=================================================================================================*
** Regressions
*=================================================================================================*
	
**********************************************************************************
********************************** Set up ****************************************
**********************************************************************************

local y d

local diff 5010
local year1 1950 
local year2 2010

use "$datafoldernew\CityShape_Main.dta", clear

foreach var of varlist * { 
	local newname = subinstr("`var'","disconnect","d",.) 
	ren `var' `newname'
}

keep if insample_IV_5010==1

**********************************************************************************
***************** Reshape and generate log difference vars ***********************
**********************************************************************************

local mylist `y'_N_km `y'_km r2_relev_`y'_cls_N_km dens_core_all 

keep id year `mylist'

foreach var of varlist `mylist' { 
	local newname = "`var'" + "_"
	ren `var' `newname' 
} 
reshape wide `mylist', i(id) j(year)

foreach j in `mylist' {
	gen `j'_`diff'=`j'_`year2'-`j'_`year1'
}


label var `y'_N_km_`diff' "D Norm. shape, km"
label var  r2_relev_`y'_cls_N_km_`diff' "D Potential norm. shape, km"

******************************************************************
******************* Run IV to generate F stat ********************
******************************************************************

clear matrix
ivreg2  dens_core_all_`diff'  (`y'_N_km_`diff'= r2_relev_`y'_cls_N_km_`diff'  ) ,  first cluster(id)
mat define fstat =e(first)
mat list fstat
local F = fstat[4,1]
local F = round(`F',.01)
local F = substr("`F'",1,strpos("`F'",".")+2)
qui local kptest = e(idstat) 
qui local kptest = round(`kptest',.01)
local kptest = substr("`kptest'",1,strpos("`kptest'",".")+2)

******************************************************************
****************** Col. 1: First stage regression ****************
******************************************************************

quie reg `y'_N_km_`diff'  r2_relev_`y'_cls_N_km_`diff'   ,  cluster(id)
qui sum `y'_N_km_2010  
local mean10 = round(r(mean),0.001) 
qui sum `y'_N_km_1950  
local mean50 = round(r(mean),0.001)
outreg2 using  "$resultsfolder/Table8_SingleInstrument.xls",  title(Table 8: First stage and impact of city shape on population, single instrument) nor2 nocons sortvar (r2_relev_`y'_cls_N_km_`diff') ctitle (First stage, Difference in norm. shape of actual footprint, `year2'-`year1') keep( r2_relev_`y'_cls_N_km_`diff' )  addte(AP F stat shape, "`F'", KP test stat, "`kptest'", Mean dep var 2010, "`mean10'", Mean dep var 1950, "`mean50'")   label(insert) replace

******************************************************************
************************** Col. 2: IV regression *****************
******************************************************************
		
ivreg2  dens_core_all_`diff'    (`y'_N_km_`diff'= r2_relev_`y'_cls_N_km_`diff'  ) ,  rf first cluster(id)
sum dens_core_all_2010 if e(sample)==1
local mean10 = round(r(mean),0.001) 
local mean10 = substr("`mean10'",1,strpos("`mean10'",".")+3)
sum dens_core_all_1950 if e(sample)==1
local mean50 = round(r(mean),0.001) 
outreg2 using "$resultsfolder/Table8_SingleInstrument.xls",  nor2 nocons  sortvar (r2_relev_`y'_cls_N_km_`diff') ctitle (IV, "Log difference of population density", `year2'-`year1') 	keep  (  `y'_N_km_`diff' r2_relev_`y'_cls_N_km_`diff' `y'_N_km_`diff') addte(AP F stat shape, "`F'", KP test stat, "`kptest'", Mean dep var 2010, "`mean10'", Mean dep var 1950, "`mean50'")   label(insert) append

******************************************************************
********************* Col. 3: OLS regression *********************
******************************************************************
quie reg dens_core_all_`diff'   `y'_N_km_`diff'  ,cluster(id)
outreg2 using "$resultsfolder/Table8_SingleInstrument.xls",  nor2 nocons  sortvar (r2_relev_`y'_cls_N_km_`diff') ctitle (OLS, "Log difference of population density", `year2'-`year1') 	keep  (  `y'_N_km_`diff' r2_relev_`y'_cls_N_km_`diff' `y'_N_km_`diff')    label(insert) append

**********************************************************************************
**********************************************************************************
**********************************************************************************
log close 

* end

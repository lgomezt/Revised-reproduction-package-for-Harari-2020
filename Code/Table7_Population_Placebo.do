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

log using "${logfolder}\Table7_Population_Placebo.log", replace 

**********************************************************************************
************************************ Tablesout ***********************************
**********************************************************************************

* Table 7: Falsification test with lagged outcomes, population				
* Table7_PlaceboPop.xls

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
***************** Reshape and generate log difference vars ***********************
**********************************************************************************

local mylist `y'_km log_area_polyg_km r1_relev_`y'_cls_km  log_projected_pop log_TOTAL_pop_all

keep id year `mylist'

foreach var of varlist `mylist' { 
	local newname = "`var'" + "_"
	ren `var' `newname' 
} 

reshape wide `mylist', i(id) j(year)

foreach j in `mylist' {
	gen `j'_5092=`j'_1992-`j'_1950
	gen `j'_5001=`j'_2001-`j'_1950
	gen `j'_0110=`j'_2010-`j'_2001
	gen `j'_9201=`j'_2001-`j'_1992
}

* Labels for regressions
label var r1_relev_d_cls_km_0110 "D Potential shape, km, 2010-2001"
label var log_projected_pop_0110 "D Log projected population, 2010-2001"

foreach j in 1995 2000 2005 2010 {
	label var r1_relev_d_cls_km_`j' "Potential shape, km, `j'"
	label var log_projected_pop_`j' "Projected population, `j'"
}

**********************************************************************************
******************************** Regressions *************************************
**********************************************************************************

* Fake regression to order variables

local sort r1_relev_d_cls_km_2005 r1_relev_d_cls_km_2010 r1_relev_d_cls_km_1995 r1_relev_d_cls_km_2000 r1_relev_d_cls_km_0110 log_projected_pop_2005 log_projected_pop_2010 log_projected_pop_1995 log_projected_pop_2000 log_projected_pop_0110

quie reg log_TOTAL_pop_all_5092  r1_relev_`y'_cls_km_0110 log_projected_pop_0110  , cluster (id)
gen insample1=e(sample)

quie reg log_TOTAL_pop_all_5092 r1_relev_`y'_cls_km_1995 log_projected_pop_1995 r1_relev_`y'_cls_km_2000 log_projected_pop_2000  , cluster (id)
gen insample2=e(sample)

quie reg log_TOTAL_pop_all_5001 r1_relev_`y'_cls_km_2005 log_projected_pop_2005 r1_relev_`y'_cls_km_2010 log_projected_pop_2010 , cluster (id)
outreg2 using "$resultsfolder/Table7_PlaceboPop.xls",  title(Table 7: Falsification test with lagged outcomes, population) sortvar (`sort')  ctitle (D log population 2001-1951) nor2 nocons label(insert) replace 

quie reg log_TOTAL_pop_all_9201 r1_relev_`y'_cls_km_2005 log_projected_pop_2005 r1_relev_`y'_cls_km_2010 log_projected_pop_2010 , cluster (id)
outreg2 using "$resultsfolder/Table7_PlaceboPop.xls",  sortvar (`sort')  ctitle (D log population 2001-1991) nor2 nocons label(insert) append 

quie reg log_TOTAL_pop_all_5092 r1_relev_`y'_cls_km_1995 log_projected_pop_1995 r1_relev_`y'_cls_km_2000 log_projected_pop_2000  if (insample1==1), cluster (id)
outreg2 using "$resultsfolder/Table7_PlaceboPop.xls",  sortvar (`sort')  ctitle (D log population 1991-1951) nor2 nocons label(insert) append 

quie reg log_TOTAL_pop_all_5092  r1_relev_`y'_cls_km_0110 log_projected_pop_0110 if (insample2==1), cluster (id)
outreg2 using "$resultsfolder/Table7_PlaceboPop.xls",  sortvar (`sort')  ctitle (D log population 1991-1951) nor2 nocons label(insert) append

**********************************************************************************
**********************************************************************************
**********************************************************************************
log close 
* end

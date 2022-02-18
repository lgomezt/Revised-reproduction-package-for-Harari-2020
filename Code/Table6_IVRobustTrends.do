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

log using "${logfolder}\Table6_IVRobustTrends.log", replace 

**********************************************************************************
************************************ Tablesout ***********************************
**********************************************************************************

* Table 6: IV impact of city shape on population, robustness to confounding trends							
* Table6_IVRobustTrends.xls

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

keep if insample_IV_5010==1

local controlsmain elevation coast_dist_km  dist_riverorlake distance_mineral_km ROUGH  bedrockdepth average_suit
replace elevation=elevation/100
label var elevation "Elevation 100 m"

local y d

local diff 5010
local year1 1950 
local year2 2010

**********************************************************************************
***************** Reshape and generate log difference vars ***********************
**********************************************************************************

local mylist `y'_km area_polyg_km log_area_polyg_km r1_relev_`y'_cls_km log_projected_pop log_TOTAL_pop_all TOTAL_pop_all

keep id year `mylist' `controlsmain'

foreach var of varlist `mylist' { 
	local newname = "`var'" + "_"
	ren `var' `newname' 
} 

reshape wide `mylist', i(id) j(year)

foreach j in `mylist' {
	gen `j'_`diff'=`j'_`year2'-`j'_`year1'
}

gen shape_`diff'=`y'_km_`diff'
label var shape_`diff' "D Shape, km"

label var r1_relev_`y'_cls_km_`diff' "D Potential shape, km"
label var `y'_km_`diff' "D Shape, km"
label var log_area_polyg_km_`diff' "D Log area"
label var log_projected_pop_`diff' "D Log projected population"

foreach control in `controlsmain'  {
	
	cap drop control
	gen control=`control'
	label var control "Control"
	
	if "`control'"=="elevation" {
		local outreg2 replace
	}
	if "`control'"!="elevation" {
		local outreg2 append
	}

	local control_label: variable label `control'	
	
	******************************************************************
	*********************** IV regression ****************************
	******************************************************************

	clear matrix
	quie ivreg2 log_TOTAL_pop_all_`diff' control (`y'_km_`diff' log_area_polyg_km_`diff' = r1_relev_`y'_cls_km_`diff' log_projected_pop_`diff' ) ,  cluster(id) first 

	mat define fstat =e(first)
	mat list fstat
	quie local SWF_`y'_km=fstat[8,1]
	quie local SWF_log_area_polyg_km=fstat[8,2]
	quie local APF_`y'_km=fstat[15,1]
	quie local APF_log_area_polyg_km=fstat[15,2]
	qui local kptest = e(idstat)
	
	local APF_`y'_km = round(`APF_`y'_km',.01)
	local APF_`y'_km = substr("`APF_`y'_km'",1,strpos("`APF_`y'_km'",".")+2)
	local APF_log_area_polyg_km = round(`APF_log_area_polyg_km',.01)
	local APF_log_area_polyg_km = substr("`APF_log_area_polyg_km'",1,strpos("`APF_log_area_polyg_km'",".")+2)
	qui local kptest = round(`kptest',.01)
	local kptest = substr("`kptest'",1,strpos("`kptest'",".")+2)
	
	outreg2 using "$resultsfolder/Table6_IVRobustTrends.xls", nor2 nocons  title(Table 6: IV impact of city shape on population, robustness to confounding trends) addtext( F stat shape,"`APF_d_km'", F stat area, "`APF_log_area_polyg_km'" , KP test stat, "`kptest'" , Control, `control_label')  ctitle (IV, Log Pop D`year2'-`year1')  label(insert) `outreg2'

}
	
**********************************************************************************
**********************************************************************************
**********************************************************************************
log close 
* end

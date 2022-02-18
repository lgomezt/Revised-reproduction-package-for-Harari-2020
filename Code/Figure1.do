version 13
cap restore
cap log close
clear all
set more off
set matsize 9000

*=================================================================================================*
** Set up
*=================================================================================================*
	
global resultsfolder 	".\Out\"
global datafoldernew 	".\Data\"
global logfolder 		".\Log"

log using "${logfolder}\Figure1_Lucas.log", replace 

*=================================================================================================*
** Regressions
*=================================================================================================*

**********************************************************************************
******************************** Figure 1 ****************************************
**********************************************************************************

use "$datafoldernew\CityShape_Main.dta", clear

*Note: Kolkata id = 457; Bangalore id = 150

keep if year == 2005 
keep if id==457 | id==150
keep spin_km range_km remoteness_km disconnect_km spin_N_km range_N_km remoteness_N_km disconnect_N_km area_polyg_km id 

gen NoN_EACradius = sqrt(area_polyg_km/_pi)
gen Norm_EACradius = NoN_EACradius 
foreach var of varlist spin* range* remoteness* disconnect* { 
	local stem = substr("`var'",1, strpos("`var'","_")-1)
	if strpos("`var'", "_N_km")>0{
		ren `var' Norm_`stem'
	} 
	else { 
		ren `var' NoN_`stem'
	}
}
drop area_polyg_km 	
reshape long NoN_ Norm_, i(id) j(metric) string
label var metric "Shape metric"
gen city = "K" if id==457 // Kolkata 
replace city = "B" if id==150 // Bangalore
drop id 
reshape wide NoN_ Norm_, i(metric) j(city) string 

levelsof NoN_K if metric == "EACradius", local(K_area) 
di "`K_area'"
gen NoN_rescale_B = Norm_B*`K_area' 
gen Adj_Diff = NoN_K - NoN_rescale_B
gen NoAdj_Diff = NoN_K - NoN_B
drop if metric == "EACradius"  

list metric Adj_Diff NoAdj_Diff

replace metric="1. Desconexión, km" if metric=="disconnect"
replace metric="2. Lejanía, km" if metric=="remoteness"
replace metric="3. Giro, km2" if metric=="spin"
replace metric="4. Rango, km" if metric=="range"

order metric NoN_K Norm_K NoN_rescale_B	Norm_B, first
keep metric NoN_K Norm_K NoN_rescale_B Norm_B
sort metric

rename metric Shape_Metric
rename NoN_K NonNormMetric_Kolkata
rename Norm_K Normalized_Kolkata
rename NoN_rescale_B NonNormMetric_Bangalore
rename Norm_B Normalized_Bangalore

export excel using "$resultsfolder\Figure1.xls", sheetreplace firstrow(variables)


log close 

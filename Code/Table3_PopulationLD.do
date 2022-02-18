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

log using "${logfolder}\Table2_FSPanel.log", replace 

**********************************************************************************
************************************ Tablesout ***********************************
**********************************************************************************

* Table 3: Impact of city shape on population		
* Table3_ShapeOnPopulation.xls

*=================================================================================================*
** Regressions
*=================================================================================================*

**********************************************************************************
********************************** Set up ****************************************
**********************************************************************************

use "$datafoldernew\CityShape_Main.dta", clear

* Sample: 351 cities
keep if insample_IV_5010==1

foreach var of varlist * { 
	local newname = subinstr("`var'","disconnect","d",.) 
	ren `var' `newname'
}

local y d

**********************************************************************************
***************** Reshape and generate log difference vars ***********************
**********************************************************************************

local diff 5010
local year1 1950 
local year2 2010

local mylist area_polyg_km `y'_N_km `y'_km  r1_relev_`y'_cls_km log_projected_pop log_area_polyg_km log_TOTAL_pop_all dens_core_all TOTAL_pop_all

keep id year `mylist'

foreach var of varlist `mylist' { 
	local newname = "`var'" + "_"
	ren `var' `newname' 
} 

reshape wide `mylist', i(id) j(year)

foreach j in `mylist' {
	gen `j'_`diff'=`j'_`year2'-`j'_`year1'
}

* Labels for regressions
label var `y'_km_`diff'	"D Shape, km"
label var log_area_polyg_km_`diff' "D Log area"
label var log_projected_pop_`diff' "D Log projected population"
label var r1_relev_`y'_cls_km_`diff' "D Potential shape, km"

**********************************************************************************
******************************** Population IV ***********************************
*********************************************************************************

ivreg2 log_TOTAL_pop_all_`diff' (`y'_km_`diff' log_area_polyg_km_`diff' = r1_relev_`y'_cls_km_`diff' log_projected_pop_`diff' ),  cluster(id) first 

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
global coef = _b[`y'_km_`diff'] 

***Calculate effect of 1SD increase in normalized shape on population 

cap program drop OneSD_Stat
prog define OneSD_Stat
{ 

	preserve 
		use "$datafoldernew\CityShape_Main.dta", clear
		keep if insample_IV_5010==1
		sum disconnect_N_km 
		local normdisconnect_sd = r(sd) 
		sum area_polyg_km
		local city_ave_area = r(mean)
	restore 

	di "************************Effect of 1SD increase*******************************" 
	di "Average city area is `city_ave_area'"
	di "One standard deviation of normalized disconnection is `normdisconnect_sd'" 
	local city_ave_radius = sqrt(`city_ave_area'/3.14) 
	di "Average city radius is `city_ave_radius' km" 
	local city_ave_normdis_sd = `city_ave_radius' * `normdisconnect_sd' 
	di "One-SD increase in normalized shape for the average-sized city in the panel is `city_ave_normdis_sd' km" 
	local coef = $coef 
	local onesd_change = `coef'*`city_ave_normdis_sd'*100 
	di "A one-SD increase in normalized shape is associated with a `onesd_change'% change in population" 
} 
end 

OneSD_Stat
outreg2 using "$resultsfolder/Table3_ShapeOnPopulation.xls", nor2 nocons title(Table 3: Impact of city shape on population) addtext(AP F stat shape,"`APF_`y'_km'", AP F stat area, "`APF_log_area_polyg_km'" , KP test stat, "`kptest'" )  ctitle (IV, Log Pop D`year2'-`year1')  label(insert) keep(log_TOTAL_pop_all_`diff' `y'_km_`diff' log_area_polyg_km_`diff'  r1_relev_`y'_cls_km_`diff' log_projected_pop_`diff' )   replace


**********************************************************************************
******************************** Population OLS **********************************
**********************************************************************************

reg log_TOTAL_pop_all_`diff' `y'_km_`diff' log_area_polyg_km_`diff' , cluster(id)

***Calculate corresponding change in shape for 1% change in population 
local onepct_pop = 1000/(_b[`y'_km_`diff']*100)
di "For a 1% increase in population, disconnection increases by `onepct_pop' meters" 

outreg2 using "$resultsfolder/Table3_ShapeOnPopulation.xls",  nor2 nocons  ctitle(OLS, Log Pop D`year2'-`year1') 	keep(`y'_km_`diff' log_area_polyg_km_`diff')  label(insert) append

**********************************************************************************
**********************************************************************************
**********************************************************************************
log close 
* end


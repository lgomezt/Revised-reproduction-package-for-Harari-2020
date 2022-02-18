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

log using "${logfolder}\Table10_EmploymentCenters.log", replace 

**********************************************************************************
************************************ Tablesout ***********************************
**********************************************************************************

* Table 10: Employment centers and work trips	
* Table10_EmploymentSubcenters.xls
	
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

label var log_avrgdis_v7_V0 "Log avg. distance to work (walk) 2010"
label var log_avrgdis_v8_V0 "Log avg. distance to work (car) 2010"
label var log_employment_subcenters "Log nr subcenters 2005"

**********************************************************************************
************************ Cols. 1 and 2: Employment centers ***********************
**********************************************************************************
	
quie ivreg2 log_employment_subcenters (`y'_km  log_area_polyg_km =  r1_relev_`y'_cls_km log_projected_pop )  if year==2005, cluster(id) first

mat define fstat =e(first)
mat list fstat
quie local SWF_`y'_km=fstat[8,1]
quie local SWF_log_area_polyg_km=fstat[8,2]
quie local APF_`y'_km=fstat[15,1]
quie local APF_log_area_polyg_km=fstat[15,2]
local APF_`y'_km = round(`APF_`y'_km',.01)
local APF_`y'_km = substr("`APF_`y'_km'",1,strpos("`APF_`y'_km'",".")+2)
local APF_log_area_polyg_km = round(`APF_log_area_polyg_km',.01)
local APF_log_area_polyg_km = substr("`APF_log_area_polyg_km'",1,strpos("`APF_log_area_polyg_km'",".")+2)
qui local kptest = e(idstat)
qui local kptest = round(`kptest',.01)
local kptest = substr("`kptest'",1,strpos("`kptest'",".")+2)

cap drop insample
gen insample=e(sample)

outreg2 using "$resultsfolder/Table10_EmploymentSubcenters.xls" , nocons nor2   addtext (AP F stat shape,"`APF_`y'_km'", AP F stat area, "`APF_log_area_polyg_km'" , KP test stat, "`kptest'") sortvar (`tosort') ctitle (IV , Log subcenters) label(insert) replace 

quie reg  log_employment_subcenters `y'_km  log_area_polyg_km if (year==2005 & insample==1), cluster(id) 
outreg2 using "$resultsfolder/Table10_EmploymentSubcenters.xls" , nocons nor2   addtext () ctitle (OLS, Log subcenters) title(Table 10: Employment centers and work trips) label(insert) append 


**********************************************************************************
************************ Cols. 3-6: Commuting distance ***************************
**********************************************************************************
	

foreach j in  log_avrgdis_v8_V0  log_avrgdis_v7_V0 {

	local jlabel:  var label `j'

	quie ivreg2  `j' (`y'_km  log_area_polyg_km =  r1_relev_`y'_cls_km log_projected_pop ) if year==2010  , cluster(id) first

	mat define fstat =e(first)
	mat list fstat
	quie local SWF_`y'_km=fstat[8,1]
	quie local SWF_log_area_polyg_km=fstat[8,2]
	quie local APF_`y'_km=fstat[15,1]
	quie local APF_log_area_polyg_km=fstat[15,2]
	local APF_`y'_km = round(`APF_`y'_km',.01)
	local APF_`y'_km = substr("`APF_`y'_km'",1,strpos("`APF_`y'_km'",".")+2)
	local APF_log_area_polyg_km = round(`APF_log_area_polyg_km',.01)
	local APF_log_area_polyg_km = substr("`APF_log_area_polyg_km'",1,strpos("`APF_log_area_polyg_km'",".")+2)
	qui local kptest = e(idstat)
	qui local kptest = round(`kptest',.01)
	local kptest = substr("`kptest'",1,strpos("`kptest'",".")+2)

	cap drop insample
	gen insample=e(sample)

	outreg2 using "$resultsfolder/Table10_EmploymentSubcenters.xls" , nocons nor2   addtext (AP F stat shape,"`APF_`y'_km'", AP F stat area, "`APF_log_area_polyg_km'" , KP test stat, "`kptest'") sortvar (`tosort') ctitle (IV , "`jlabel'") label(insert) append

	quie reg  `j'  `y'_km  log_area_polyg_km if insample==1, cluster(id) 
	outreg2 using "$resultsfolder/Table10_EmploymentSubcenters.xls" , nocons nor2   addtext () ctitle (OLS, "`jlabel'")  label(insert) append 

}

**********************************************************************************
**********************************************************************************
**********************************************************************************
log close 
* end

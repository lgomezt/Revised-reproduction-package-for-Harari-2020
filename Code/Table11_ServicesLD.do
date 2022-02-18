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

global resultsfolder ".\Out\"
global datafoldernew ".\Data\"
global logfolder 		".\Log"

log using "${logfolder}\Table11_ServicesLD.log", replace 

**********************************************************************************
************************************ Tablesout ***********************************
**********************************************************************************

* Table 11: Impact of city shape on public services											
* Table11_Services.xls

*=================================================================================================*
** Regressions
*=================================================================================================*
	
**********************************************************************************
********************************** Set up ****************************************
**********************************************************************************

local y d

local diff 9210
local year1 1992 
local year2 2010

use "$datafoldernew\CityShape_Main.dta", clear

foreach var of varlist * { 
	local newname = subinstr("`var'","disconnect","d",.) 
	ren `var' `newname'
}

**********************************************************************************
***************** Reshape and generate log difference vars ***********************
**********************************************************************************

local mylist log_share_elec_tot log_share_tap_tot log_nrh_elec_tot log_nrh_tap_tot log_projected_pop  `y'_km area_polyg_km log_area_polyg_km r1_relev_`y'_cls_km 

keep id year `mylist' 

foreach var of varlist `mylist' { 
	local newname = "`var'" + "_"
	ren `var' `newname' 
} 

reshape wide `mylist', i(id) j(year)

foreach j in `mylist' {
	gen `j'_`diff'=`j'_`year2'-`j'_`year1'
}

label var `y'_km_`diff' "D Shape, km"
label var log_area_polyg_km_`diff' "D Log area"
label var log_nrh_elec_tot_`diff' "D Log nr households, 2011-1991"
label var log_nrh_tap_tot_`diff' "D Log nr households, 2011-1991"
label var log_share_tap_tot_`diff' "D Log share households, 2011-1991"
label var log_share_elec_tot_`diff' "D Log share households, 2011-1991"

**********************************************************************************
************************************ IV ******************************************
**********************************************************************************

foreach k in log_nrh_elec_tot log_share_elec_tot log_nrh_tap_tot log_share_tap_tot {

	local labelvar: var label `k'_`diff'

	if "`k'"=="log_nrh_elec_tot" {
		local outreg2 replace
		local labelcol "A. Electricity"
	}
	if "`k'"=="log_share_elec_tot" {
		local outreg2 append
		local labelcol "A. Electricity"
	}
	if ("`k'"=="log_nrh_tap_tot" | "`k'"=="log_nrh_tap_tot") {
		local outreg2 append
		local labelcol "B. Tap"
	}

	clear matrix
	quie ivreg2 `k'_`diff' (d_km_`diff' log_area_polyg_km_`diff' = r1_relev_d_cls_km_`diff' log_projected_pop_`diff' ) ,  cluster(id) first 

	mat define fstat =e(first)
	mat list fstat
	quie local SWF_d_km=fstat[8,1]
	quie local SWF_log_area_polyg_km=fstat[8,2]
	quie local APF_d_km=fstat[15,1]
	quie local APF_log_area_polyg_km=fstat[15,2]
	qui local kptest = e(idstat)
	
	local APF_d_km = round(`APF_d_km',.01)
	local APF_d_km = substr("`APF_d_km'",1,strpos("`APF_d_km'",".")+2)
	local APF_log_area_polyg_km = round(`APF_log_area_polyg_km',.01)
	local APF_log_area_polyg_km = substr("`APF_log_area_polyg_km'",1,strpos("`APF_log_area_polyg_km'",".")+2)
	qui local kptest = round(`kptest',.01)
	local kptest = substr("`kptest'",1,strpos("`kptest'",".")+2)

	outreg2 using "$resultsfolder/Table11_Services.xls", title(Table 11: Impact of city shape on public services) nor2 nocons addtext(AP F stat shape,"`APF_d_km'", AP F stat area, "`APF_log_area_polyg_km'" , KP test stat, "`kptest'")  ctitle("`labelcol'", "`labelvar'", IV)  label(insert) `outreg2' 


	**********************************************************************************
	************************************ OLS *****************************************
	**********************************************************************************

   	quie reg `k'_`diff' d_km_`diff' log_area_polyg_km_`diff',  cluster(id) 
	outreg2 using "$resultsfolder/Table11_Services.xls", nor2 nocons addtext() ctitle("`labelcol'", "`labelvar'", OLS) label(insert) append


}
	
**********************************************************************************
**********************************************************************************
**********************************************************************************
log close 
* end

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
cd "C:\Users\User\OneDrive - Universidad de los Andes\12. Último semestre\Urban economics\Final project\Plan C. Economía"

global resultsfolder 	".\Revised reproduction package for Harari, 2020\Out\"
global datafoldernew 	".\ReplicationFolder_Main\Data\"
global logfolder 		".\ReplicationFolder_Main\Log"

log using "${logfolder}\Table2_FSLD.log", replace 

**********************************************************************************
************************************ Tablesout ***********************************
**********************************************************************************

* Table 2: First stage, long difference (columns 1 and 2)
* Table2_Cols12_FS.xls

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
********************************* First Stage ************************************
**********************************************************************************

************* Run xtivreg regression to get first stage F & KP stat ************

quie ivreg2 log_TOTAL_pop_all_`diff'  (`y'_km_`diff' log_area_polyg_km_`diff' = r1_relev_`y'_cls_km_`diff' log_projected_pop_`diff' )  , first cluster(id)

mat define fstat =e(first)
mat list fstat
quie local SWF_`y'_km=fstat[8,1]
quie local SWF_log_area_polyg_km=fstat[8,2]
quie local APF_`y'_km=fstat[15,1]
quie local APF_log_area_polyg_km=fstat[15,2]
qui local kptest=e(idstat)

local APF_`y'_km = round(`APF_`y'_km',.01)
local APF_`y'_km = substr("`APF_`y'_km'",1,strpos("`APF_`y'_km'",".")+2)
local APF_log_area_polyg_km = round(`APF_log_area_polyg_km',.01)
local APF_log_area_polyg_km = substr("`APF_log_area_polyg_km'",1,strpos("`APF_log_area_polyg_km'",".")+2)
qui local kptest = round(`kptest',.01)
local kptest = substr("`kptest'",1,strpos("`kptest'",".")+2)

**************************** First stage for shape *****************************

quie reg `y'_km_`diff'  r1_relev_`y'_cls_km_`diff' log_projected_pop_`diff', cluster(id)
outreg2 using "$resultsfolder/Table2_Cols12_FS.xls", nor2 nocons title(Table 2: First stage) addtext(AP F stat shape, "`APF_`y'_km'", AP F stat area, "`APF_log_area_polyg_km'" , KP test stat, "`kptest'") ctitle(OLS, Disconnection D`year2'-`year1') label(insert) keep(r1_relev_`y'_cls_km_`diff' log_projected_pop_`diff' r1_relev_`y'_cls_km_`diff' ) replace 

**************************** First stage for area *****************************

quie reg log_area_polyg_km_`diff'  r1_relev_`y'_cls_km_`diff' log_projected_pop_`diff', cluster(id)
outreg2 using "$resultsfolder/Table2_Cols12_FS.xls", nor2 nocons addtext(AP F stat shape, "`APF_`y'_km'", AP F stat area, "`APF_log_area_polyg_km'" , KP test stat, "`kptest'")  ctitle (OLS, Log area D`year2'-`year1')  label(insert) keep( log_projected_pop_`diff' r1_relev_`y'_cls_km_`diff')   append 

**********************************************************************************
**********************************************************************************
**********************************************************************************
log close 
* end


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

log using "${logfolder}\Table2_FSPanel.log", replace 
**********************************************************************************
************************************ Tablesout ***********************************
**********************************************************************************

* Table 2, columns 3 and 4: First stage, panel
* Table2_Panel_FirstStage.xls

*=================================================================================================*
** Regressions
*=================================================================================================*

**********************************************************************************
********************************** Set up ****************************************
**********************************************************************************

use "$datafoldernew\CityShape_Main.dta", clear

* Sample: 351 cities
keep if insample_IV_5010==1

local y disconnect

**********************************************************************************
*************************** First Stage regressions ******************************
**********************************************************************************

/* Note:
Cols. 3 and 4 report first stage F stats for the full sample of city-years for which shape and area are observed.
This includes years for which there is no population data. An IV regression with population as an outcome would exclude those years.
In order to compute the first stage F stats for this sample, create a temporary dependent variable that is never missing 
and use it to run an IV regression for the sole purpose of retrieveing the F stats. 
*/

label var r1_relev_`y'_cls_km "Potential shape, km"

************* Run IV regression to get first stage F & KP stat ************

gen temp =`y'_km * 2 
quie ivreg2 temp i.id i.year (`y'_km log_area_polyg_km = r1_relev_`y'_cls_km log_projected_pop )  , cluster(id) first

mat define fstat = e(first)
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

**************************** First stage for shape *****************************

quie reg `y'_km log_projected_pop r1_relev_`y'_cls_km i.year i.id ,  cluster(id)
outreg2 using "$resultsfolder/Table2_Panel_FirstStage(Stata).xls", title(Table 2, columns 3 and 4: First stage, panel) nor2 nocons addtext(AP F stat shape, "`APF_`y'_km'", AP F stat area, "`APF_log_area_polyg_km'" , KP F test stat, "`kptest'", City FE, Y, Year FE, Y) ctitle (OLS, "Shape, km") label(insert) keep(r1_relev_`y'_cls_km log_projected_pop r1_relev_`y'_cls_km ) replace

**************************** First stage for area *****************************

quie reg log_area_polyg_km log_projected_pop r1_relev_`y'_cls_km i.year i.id,  cluster(id)
outreg2 using "$resultsfolder/Table2_Panel_FirstStage(Stata).xls", nor2 nocons addtext(AP F stat shape, "`APF_`y'_km'", AP F stat area, "`APF_log_area_polyg_km'" , KP F test stat, "`kptest'", City FE, Y, Year FE, Y) ctitle (OLS, "Log area, km") label(insert) keep(r1_relev_`y'_cls_km log_projected_pop r1_relev_`y'_cls_km ) append

log close 
* end

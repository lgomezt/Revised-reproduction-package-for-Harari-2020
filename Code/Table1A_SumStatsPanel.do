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

**********************************************************************************
************************************ Tablesout ***********************************
**********************************************************************************

* Table 1, panel A: Descriptive statistics, Panel 
* Table1PanelA_Descriptive.log

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

********************************************************************************
************ Descriptive statistics for shape and area variables ***************
********************************************************************************

log using "${logfolder}/Table1PanelA_Descriptive.log" , replace

	tabstat area_polyg_km `y'_km  r1_relev_`y'_cls_km , stat(count mean median sd min max) save
	
	* Avg SD in shape within a city over years

	egen SD = sd( disconnect_km ) if area_polyg_km!=., by(id)
	sum SD
	
log close

********************************************************************************
************ Descriptive statistics, population sample *************************
********************************************************************************

* Run population OLS to define sample
quie reg log_TOTAL_pop_all i.id i.year `y'_km log_area_polyg_km , cluster(id)

log using "$resultsfolder/Table1PanelA_Descriptive.log" , append

	tabstat TOTAL_pop_all if e(sample)==1, stat(count mean median sd min max) save
	
log close
**********************************************************************************
**********************************************************************************
**********************************************************************************

* end

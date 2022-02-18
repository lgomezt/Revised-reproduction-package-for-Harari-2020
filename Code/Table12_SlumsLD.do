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

log using "${logfolder}\Table12_SlumsLD.log", replace 

**********************************************************************************
************************************ Tablesout ***********************************
**********************************************************************************

* Table 12: Impact of city shape on slum population					
* Table12_Slums.xls
	
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

local mylist log_TOT_P_Slum  log_NPURB2_TOT_P_Slum `y'_km log_area_polyg_km r1_relev_`y'_cls_km log_projected_pop

keep year id `mylist'

reshape wide `mylist', j(year) i(id)

*create log diff variables 
foreach var in `mylist'  {
	gen `var'_5010 = `var'2010 - `var'1950
}	

* Labels for regressions
label var `y'_km_5010 "D Shape, km"
label var log_area_polyg_km_5010 "D Area, km"

**********************************************************************************
******************************** IV and OLS **************************************
**********************************************************************************

cap erase "$resultsfolder/Table12_Slums.xls"
cap erase "$resultsfolder/Table12_Slums.txt"

foreach k in log_TOT_P_Slum_5010 log_NPURB2_TOT_P_Slum_5010 {
	
	if "`k'" == "log_TOT_P_Slum_5010" {
		local outreg_v "replace"
		local labely "D Log slum population, 2011-1981"	
	}
	
	if "`k'" == "log_NPURB2_TOT_P_Slum_5010" {
		local outreg_v "append"
		local labely "D Log slum population share, 2011-1981"	
	}

	******************************* IV **********************************

	clear matrix
		
	ivreg2 `k' (`y'_km_5010 log_area_polyg_km_5010 = r1_relev_`y'_cls_km_5010 log_projected_pop_5010) , first cluster(id)
		
	mat define fstat =e(first)
	mat list fstat
	quie local SWF_`y'_km_`diff'=fstat[8,1]
	quie local SWF_log_area_polyg_km_`diff'=fstat[8,2]
	quie local APF_`y'_km_`diff'=fstat[15,1]
	quie local APF_log_area_polyg_km_`diff'=fstat[15,2]
	local APF_`y'_km_`diff' = round(`APF_`y'_km_`diff'',.01)
	local APF_`y'_km_`diff' = substr("`APF_`y'_km_`diff''",1,strpos("`APF_`y'_km_`diff''",".")+2)
	local APF_log_area_polyg_km_`diff' = round(`APF_log_area_polyg_km_`diff'',.01)
	local APF_log_area_polyg_km_`diff' = substr("`APF_log_area_polyg_km_`diff''",1,strpos("`APF_log_area_polyg_km_`diff''",".")+2)
	qui local kptest = e(idstat)
	qui local kptest = round(`kptest',.01)
	local kptest = substr("`kptest'",1,strpos("`kptest'",".")+2)
	qui local kpval = e(idp)

	outreg2 using "$resultsfolder/Table12_Slums.xls" , title(Table 12: Impact of city shape on slum population) alpha (0.01,  0.05 , 0.1 ,  0.15) symbol (*** , ** , *, +) nocons ctitle ("`labely', IV") addte(AP F stat shape, "`APF_`y'_km_`diff''", AP F stat area, "`APF_log_area_polyg_km_`diff''", KP test stat, "`kptest'")   label(insert)  nor2  `outreg_v'
	gen insample=e(sample)
	
	******************************* OLS ********************************
	qui reg `k' `y'_km_5010 log_area_polyg_km_5010 if insample==1, cluster(id)
	outreg2 using "$resultsfolder/Table12_Slums.xls" , alpha (0.01,  0.05 , 0.1 ,  0.15) symbol (*** , ** , *, +) nocons ctitle ("`labely', OLS")   label(insert)  nor2   append
	
	drop insample
	
}



**********************************************************************************
**********************************************************************************
**********************************************************************************
log close 
* end

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

log using "${logfolder}\Table4_WagesLD.log", replace 

**********************************************************************************
************************************ Tablesout ***********************************
**********************************************************************************

* Table 4: Impact of city shape on wages				
* Table4_Wages.xls
	

*=================================================================================================*
** Regressions
*=================================================================================================*

**********************************************************************************
********************************** Set up ****************************************
**********************************************************************************

use "$datafoldernew\CityShape_Main.dta", clear

keep if insample_FS_FullPanel==1

foreach var of varlist * { 
	local newname = subinstr("`var'","disconnect","d",.) 
	ren `var' `newname'
}

local y d

**********************************************************************************
***************** Reshape and generate log difference vars ***********************
**********************************************************************************

* Note: wages are from 1990 ASI, but are assigned to year 1992 in the panel (the closest year for which shapes are observed).
local diff 9210
local year1 1992 
local year2 2010

local mylist `y'_km log_area_polyg_km r1_relev_`y'_cls_km log_projected_pop per_worker_wage_Md_V0 per_worker_wage_Md_V1 per_worker_wage_Md_V2 log_TOTAL_pop_all

keep id year `mylist'

foreach var of varlist per_worker_wage_Md_V0  per_worker_wage_Md_V1   per_worker_wage_Md_V2{ 
	gen log_`var' = log(`var') 
} 

local mylist `mylist' log_per_worker_wage_Md_V0 log_per_worker_wage_Md_V1 log_per_worker_wage_Md_V2

foreach var of varlist `mylist' { 
	local newname = "`var'" + "_"
	ren `var' `newname' 
} 
reshape wide `mylist', i(id) j(year)

foreach j in `mylist' {
	gen `j'_`diff'=`j'_`year2'-`j'_`year1'
}

foreach j in `mylist' {
	gen `j'_5010=`j'_2010-`j'_1950
}

* Labels for regressions
label var `y'_km_`diff'	"D Shape, km"
label var log_area_polyg_km_`diff' "D Log area"
label var log_projected_pop_`diff' "D Log projected population"
label var r1_relev_`y'_cls_km_`diff' "D Potential shape, km"

**********************************************************************************
******************************** IV and OLS **************************************
**********************************************************************************

cap erase "$resultsfolder/Table4_Wages.xls"
cap erase "$resultsfolder/Table4_Wages.txt"

foreach k in per_worker_wage_Md_V0  per_worker_wage_Md_V1   per_worker_wage_Md_V2 {


	if strpos("`k'","_v0")>0 {
			local districts_obs="All districts"
			local outreg_v "replace"
		}
	if strpos("`k'","_v1")>0 {
			local districts_obs="Only districts with one city"
			local outreg_v "append"
		}
	if strpos("`k'","_v2")>0 {
			local districts_obs="Only top city per district"
			local outreg_v "append"
		}


	clear matrix
	quie ivreg2 log_`k'_`diff' (`y'_km_`diff' log_area_polyg_km_`diff' = r1_relev_`y'_cls_km_`diff' log_projected_pop_`diff' ) ,  first cluster(id)

	cap drop insample
	gen insample=e(sample)

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

	disp "SW stats, shape & area:"
	disp  `SWF_`y'_km_`diff''
	disp  `SWF_log_area_polyg_km_`diff''

	disp "AP stats, shape & area:"
	disp  `APF_`y'_km_`diff''
	disp  `APF_log_area_polyg_km_`diff''

	sum `k'_1992 if insample==1
	qui local mean = `r(mean)'
	qui local mean = round(`mean',1)

	sum `k'_2010 if insample==1
	qui local mean2 = `r(mean)'
	qui local mean2 = round(`mean2',1)

	di "***************Calculating implied productivity impacts: IV *****************"
	preserve 
		quie ivreg2 log_TOTAL_pop_all_5010 (`y'_km_5010 log_area_polyg_km_5010 = r1_relev_`y'_cls_km_5010 log_projected_pop_5010 ),  cluster(id) first 
		est sto pop  
		qui keep if e(sample) == 1
		rename log_TOTAL_pop_all_5010 LHS
		rename d_km_5010 shape
		rename log_area_polyg_km_5010 area
		rename r1_relev_d_cls_km_5010 IV_shape
		rename log_projected_pop_5010 IV_area
		keep LHS shape area IV_shape IV_area id
		gen wage_samp = 0
		tempfile t_pop
		save `t_pop'
	restore 

	preserve 
		quie ivreg2 log_`k'_9210 (`y'_km_9210 log_area_polyg_km_9210 = r1_relev_`y'_cls_km_9210 log_projected_pop_9210 ) ,  first cluster(id)
		est sto wages
		qui keep if e(sample) == 1
		rename log_`k'_9210 LHS
		rename d_km_9210 shape
		rename log_area_polyg_km_9210 area
		rename r1_relev_d_cls_km_9210 IV_shape
		rename log_projected_pop_9210 IV_area
		keep LHS shape area IV_shape IV_area id
		gen wage_samp = 1

		*append two samples and gen interaction terms
		qui append using `t_pop'
		gen shape_diff = shape*wage_samp
		gen IV_shape_diff = IV_shape*wage_samp
		gen area_diff = area*wage_samp
		gen IV_area_diff = IV_area*wage_samp

		*pooling iv reg
		qui ivreg2 LHS (shape area shape_diff area_diff = IV_shape IV_area IV_shape_diff IV_area_diff) wage_samp, cluster(id)
		est sto stacked
		
		esttab pop wages stacked, nogaps mti
		*Note: _b[shape] + _b[shape_diff] = shape_wages
		*	   _b[shape] = shape_pop 
			
		*Compute
		local beta = 0.4 
		local gamma = 0.3 
		lincom (1-`beta'-`gamma')*_b[shape] + (1-`gamma')*(_b[shape_diff] + _b[shape])
		local pi = round(r(estimate),0.001)
		local pi_se = round(r(se),0.001)
		local pi_p = round(2*ttail(2e+17,abs(r(estimate)/r(se))),0.001)
		
	restore 

	

	******************************* IV ********************************
	quie ivreg2 log_`k'_`diff' (`y'_km_`diff' log_area_polyg_km_`diff' = r1_relev_`y'_cls_km_`diff' log_projected_pop_`diff' ) ,  first cluster(id)
	outreg2 using "$resultsfolder/Table4_Wages.xls" , title(Table 4: Impact of city shape on wages) alpha (0.01,  0.05 , 0.1 ,  0.15) symbol (*** , ** , *, +) nocons ctitle (IV, `districts_obs', `year2'-`year1') addte(AP F stat shape, "`APF_`y'_km_`diff''", AP F stat area, "`APF_log_area_polyg_km_`diff''", KP test stat, "`kptest'", Avg. yearly wage 1992, `mean', Avg. yearly wage 2010, `mean2')  label(insert) keep (`y'_km_`diff' log_area_polyg_km_`diff' r1_relev_`y'_cls_km_`diff' log_projected_pop_`diff' ) nor2   `outreg_v'

	******************************* OLS ********************************

	quie reg log_`k'_`diff' `y'_km_`diff' log_area_polyg_km_`diff'  if insample==1, cluster(id)
	outreg2 using "$resultsfolder/Table4_Wages.xls" , sortvar (`tosort') alpha (0.01 , 0.05 , 0.1 ,  0.15) symbol (*** , ** , *, +) nocons ctitle (OLS,`districts_obs', `year2'-`year1' ) label(insert) keep (`y'_km_`diff' log_area_polyg_km_`diff') addte(Avg. yearly wage 1992, `mean', Avg. yearly wage 2010, `mean2')  nor2 `outreg_v'

}

**********************************************************************************
**********************************************************************************
**********************************************************************************
log close 
* end

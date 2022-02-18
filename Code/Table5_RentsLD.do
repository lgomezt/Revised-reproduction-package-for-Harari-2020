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

log using "${logfolder}\Table5_RentsLD.log", replace 

**********************************************************************************
************************************ Tablesout ***********************************
**********************************************************************************

* Table 5: Impact of city shape on rents
* Table5_Rents.xls

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

local diff 0608
local year1 2006 
local year2 2008

local mylist `y'_km log_area_polyg_km r1_relev_`y'_cls_km  log_projected_pop  rent_1_Mt_v0 rent_1_Mt_v1 rent_1_Mt_v2 per_worker_wage_Md_V0  per_worker_wage_Md_V1   per_worker_wage_Md_V2

keep id year `mylist'

foreach var of varlist  rent_1_Mt_v0 rent_1_Mt_v1 rent_1_Mt_v2 per_worker_wage_Md_V0  per_worker_wage_Md_V1   per_worker_wage_Md_V2{ 
	gen log_`var' = log(`var') 
} 
drop per_worker_wage_Md_V0  per_worker_wage_Md_V1   per_worker_wage_Md_V2 

local mylist `y'_km log_area_polyg_km r1_relev_`y'_cls_km  log_projected_pop  rent_1_Mt_v0 rent_1_Mt_v1 rent_1_Mt_v2 log_rent_1_Mt_v0 log_rent_1_Mt_v1 log_rent_1_Mt_v2  ///
	log_per_worker_wage_Md_V0 log_per_worker_wage_Md_V1 log_per_worker_wage_Md_V2
	
foreach var of varlist `mylist' { 
	local newname = "`var'" + "_"
	ren `var' `newname' 
} 

reshape wide `mylist', i(id) j(year)

foreach j in `mylist' {
	gen `j'_`diff'=`j'_`year2'-`j'_`year1'
}

foreach j in `mylist' { 
	gen `j'_9210 = `j'_2010 - `j'_1992 
} 

rename log_per_worker_wage_Md_V0_9210 log_per_worker_wage_Md_v0_9210
rename log_per_worker_wage_Md_V1_9210 log_per_worker_wage_Md_v1_9210
rename log_per_worker_wage_Md_V2_9210 log_per_worker_wage_Md_v2_9210

* Labels for regressions
label var `y'_km_`diff'	"D Shape, km"
label var log_area_polyg_km_`diff' "D Log area"
label var log_projected_pop_`diff' "D Log projected population"
label var r1_relev_`y'_cls_km_`diff' "D Potential shape, km"

*Note: V0, V1, V2 correspond to all districts, only districts with one city, only top city per dstrict

**********************************************************************************
******************************** IV and OLS **************************************
**********************************************************************************

foreach j in Table5_Rents{
	cap erase "$resultsfolder/`j'.xls"
	cap erase "$resultsfolder/`j'.txt"
}


foreach k in rent_1_Mt_v0 rent_1_Mt_v1 rent_1_Mt_v2 {


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


	if strpos("`k'","Q")==0 {
		local tablename "Table5_Rents"
		local tabletitle "Table 5: Impact of city shape on rents"
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

	sum `k'_2006 if insample==1
	qui local mean = `r(mean)'
	qui local mean = round(`mean',1)

	********************************************************************
	********************* Calculating willingness to pay ***************
	********************************************************************
	
	di "********************************* IV *******************************"

	*version 
	local version = substr("`k'",-2,.) 
	
	*wages est samp
	preserve 
		qui ivreg2 log_per_worker_wage_Md_`version'_9210 (d_km_9210 log_area_polyg_km_9210 = r1_relev_d_cls_km_9210 log_projected_pop_9210), cluster(id)
		est sto wages
		qui keep if e(sample) == 1
		rename log_per_worker_wage_Md_`version'_9210 LHS
		rename d_km_9210 shape
		rename log_area_polyg_km_9210 area
		rename r1_relev_d_cls_km_9210 IV_shape
		rename log_projected_pop_9210 IV_area
		keep LHS shape area IV_shape IV_area id
		gen rents_samp = 0
		tempfile t_wages
		save `t_wages'
	restore 

	preserve 
		qui ivreg2 log_rent_1_Mt_`version'_0608 (d_km_0608 log_area_polyg_km_0608 = r1_relev_d_cls_km_0608 log_projected_pop_0608), cluster(id)
		est sto rents
		qui keep if e(sample) == 1
		rename log_rent_1_Mt_`version'_0608 LHS
		rename d_km_0608 shape
		rename log_area_polyg_km_0608 area
		rename r1_relev_d_cls_km_0608 IV_shape
		rename log_projected_pop_0608 IV_area
		keep LHS shape area IV_shape IV_area id
		gen rents_samp = 1

		*append two samples and gen interaction terms
		qui append using `t_wages'
		gen shape_diff = shape*rents_samp
		gen IV_shape_diff = IV_shape*rents_samp
		gen area_diff = area*rents_samp
		gen IV_area_diff = IV_area*rents_samp

		*pooling iv reg
		qui ivreg2 LHS (shape area shape_diff area_diff = IV_shape IV_area IV_shape_diff IV_area_diff) rents_samp, cluster(id)
		est sto stacked
		
		esttab wages rents stacked, nogaps mti
		*recover point estimates for rents reg
		*di "construct the point estimate of shape for rents reg"
		*lincom _b[shape] + _b[shape_diff]
		*di "construct the point estimate of area for rents reg"
		*lincom _b[area] + _b[area_diff]

		*test
		di "`version': test if 0.16*shape_rents = shape_wages"
		test (0.16*(_b[shape] + _b[shape_diff]) = _b[shape])	
		*di "`version': test if 0.16*area_rents = area_wages"
		*test (0.16*(_b[area] + _b[area_diff]) = _b[area])	
		
		*confidence interval 
		di "`version': confidence interval"
		lincom 0.16*(_b[shape] + _b[shape_diff]) - _b[shape]
		local wtp = round(r(estimate),0.001)
		di "wtp is `wtp'" 
		local wtp_se = round(r(se),0.001)
		local wtp_p = round(2*ttail(2e+17,abs(r(estimate)/r(se))),0.001) 
	restore 
	
	
	*******************************************************************
	*******************   IV REGRESSION FOR DISPLAY *******************
	*******************************************************************
	quie ivreg2 log_`k'_`diff' (`y'_km_`diff' log_area_polyg_km_`diff' = r1_relev_`y'_cls_km_`diff' log_projected_pop_`diff' ) ,  first cluster(id)
	outreg2 using "$resultsfolder/`tablename'.xls" ,  title(`tabletitle') alpha (0.01,  0.05 , 0.1 ,  0.15) symbol (*** , ** , *, +) nocons ctitle (IV, `districts_obs', `year2'-`year1') addte(AP F stat shape, "`APF_`y'_km_`diff''", AP F stat area, "`APF_log_area_polyg_km_`diff''", KP test stat, "`kptest'", Avg. yearly rent per m2 2006, "`mean'", Willingness to pay, "`wtp'", WTP SE, "`wtp_se'", WTP p-value,"`wtp_p'" )  label(insert) keep (`y'_km_`diff' log_area_polyg_km_`diff' r1_relev_`y'_cls_km_`diff' log_projected_pop_`diff' ) nor2   `outreg_v'

		
	di "********************************* OLS *******************************"
	
	*version 
	local version = substr("`k'",-2,.) 
	
	*wages est samp
	preserve 
		qui ivreg2 log_per_worker_wage_Md_`version'_9210 (d_km_9210 log_area_polyg_km_9210 = r1_relev_d_cls_km_9210 log_projected_pop_9210), cluster(id)
		gen insample_pop = e(sample)
		qui reg log_per_worker_wage_Md_`version'_9210 d_km_9210 log_area_polyg_km_9210, cluster(id) 
		est sto wages 
		qui keep if e(sample) == 1
		rename log_per_worker_wage_Md_`version'_9210 LHS
		rename d_km_9210 shape
		rename log_area_polyg_km_9210 area
		keep LHS shape area id
		gen rents_samp = 0
		tempfile t_wages
		save `t_wages'
	restore 

	preserve 
		qui ivreg2 log_rent_1_Mt_`version'_0608 (d_km_0608 log_area_polyg_km_0608 = r1_relev_d_cls_km_0608 log_projected_pop_0608), cluster(id)
		gen insample_pop = e(sample)
		qui reg log_rent_1_Mt_`version'_0608 d_km_0608 log_area_polyg_km_0608, cluster(id)
		est sto rents
		qui keep if e(sample) == 1
		rename log_rent_1_Mt_`version'_0608 LHS
		rename d_km_0608 shape
		rename log_area_polyg_km_0608 area
		keep LHS shape area id
		gen rents_samp = 1

		*append two samples and gen interaction terms
		qui append using `t_wages'
		gen shape_diff = shape*rents_samp
		gen area_diff = area*rents_samp

		*pooling iv reg
		qui reg LHS shape area shape_diff area_diff rents_samp, cluster(id)
		est sto stacked
		
		esttab wages rents stacked, nogaps mti
		*recover point estimates for rents reg
		*di "construct the point estimate of shape for rents reg"
		*lincom _b[shape] + _b[shape_diff]
		*di "construct the point estimate of area for rents reg"
		*lincom _b[area] + _b[area_diff]

		*test
		di "`version': test if 0.16*shape_rents = shape_wages"
		test (0.16*(_b[shape] + _b[shape_diff]) = _b[shape])	
		*di "`version': test if 0.16*area_rents = area_wages"
		*test (0.16*(_b[area] + _b[area_diff]) = _b[area])	
		
		*confidence interval 
		di "`version': confidence interval"
		lincom 0.16*(_b[shape] + _b[shape_diff]) - _b[shape]
		local wtp = round(r(estimate),0.001)
		di "OLS wtp is `wtp'" 
		local wtp_se = round(r(se),0.001)
		local wtp_p = round(2*ttail(2e+17,abs(r(estimate)/r(se))),0.001) 
	restore 
	
	****Calculate WTP for a one-sd improvement in city compactness*****
	preserve 
		use "$datafoldernew\CityShape_Main.dta", clear
		keep if insample_IV_5010==1
		sum disconnect_N_km 
		local normdisconnect_sd = r(sd) 
		sum area_polyg_km
		local city_ave_area = r(mean)
	restore 

	di "************************1SD increase*******************************" 
	di "Average city area is `city_ave_area'"
	di "One standard deviation of normalized disconnection is `normdisconnect_sd'" 
	local city_ave_radius = sqrt(`city_ave_area'/3.14) 
	di "Average city radius is `city_ave_radius' km" 
	local city_ave_normdis_sd = `city_ave_radius' * `normdisconnect_sd' 
	di "One-SD increase in normalized shape for the average-sized city in the panel is `city_ave_normdis_sd' km" 
	local wtp_onesd = `city_ave_normdis_sd'*(-`wtp') 
	di "A one-sd improvement in city compactness implies a willingness to pay of `wtp_onesd'" 
	
	di "*******Estimate cost of covering +360m on foot twice a day*********"
	local metersperday = `city_ave_normdis_sd'*2 
	local avgspeed_onfoot_hr = 5 
	local additional_traveltime_min = `metersperday'/`avgspeed_onfoot_hr'*60 
	di "Covering 360 additional meters on foot twice a day takes `additional_traveltime_min' minutes" 
	local workday_length_min = 8*60 
	local percent_workday = `additional_traveltime_min'/`workday_length_min'*100
	di "Covering 360 additional meters takes `percent_workday'% of an 8-hour working day" 

	*******************************************************************
	******************* OLS REGRESSIONS FOR DISPLAY *******************
	*******************************************************************
	
	quie reg log_`k'_`diff' `y'_km_`diff' log_area_polyg_km_`diff'  if insample==1, cluster(id)
	outreg2 using "$resultsfolder/`tablename'.xls" , sortvar (`tosort') alpha (0.01 , 0.05 , 0.1 ,  0.15) symbol (*** , ** , *, +) nocons ctitle (OLS,`districts_obs', `year2'-`year1' ) label(insert) keep (`y'_km_`diff' log_area_polyg_km_`diff') addte(Avg. yearly rent per m2 2006, "`mean'") nor2 append

	}


**********************************************************************************
**********************************************************************************
**********************************************************************************
log close 
* end

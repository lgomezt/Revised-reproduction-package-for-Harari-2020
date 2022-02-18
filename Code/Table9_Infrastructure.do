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

global resultsfolder	 ".\Out\"
global datafoldernew 	".\Data\"
global logfolder 		".\Log"

log using "${logfolder}\Table9_Infrastructure.log", replace 

**********************************************************************************
************************************ Tablesout ***********************************
**********************************************************************************

* Table 9: Heterogeneous effects of infrastructure and transit 								
* Table9_Infrastructure.xls

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


keep if insample_IV_5010==1

local y d

local diff 5010
local year1 1950 
local year2 2010

************************************************************************************

* Roads
local panelA1 OSM_roads_all_2010 C_total_roads_km_1981 URL_perarea2_1981 

* Akbar et al indices
local panelA2  AFE2_prox_6   Grids

* Motor vehicles
local panelB nrh_car_tot_NH_2010  nr_hh_car_2001  nrv_perarea2_1984

* Express nr of households with cars in thousands
foreach h in nrh_car_tot_NH_2010 nr_hh_car_2001  {
replace `h'=`h'/1000
}

label var nrh_car_tot_NH_2010  "Cars 2011"
label var nr_hh_car_2001 "Cars 2001"
label var nrv_perarea2_1984 "State cars 1984"

label var grid_orient_pst_2_none_1 "Grid conformity"
label var AFE2_prox_6 "Proximity"
label var OSM_roads_all_2010 "Roads 2019"
label var C_total_roads_km_1981 "Roads 1981"
label var URL_perarea2_1981 "State roads 1981"


************* Create shorter varnames

rename grid_orient_pst_2_none_1 Grids

* Shape instrument
rename r1_relev_`y'_cls_km potshape
* Population instrument 
rename log_projected_pop lpop

cap erase  "$resultsfolder/Table9_Infrastructure.xls" 
cap erase  "$resultsfolder/Table9_Infrastructure.txt" 

	
foreach panel in panelA1 panelA2 panelB  {

	**********************************************************************************
	***************** Reshape and generate log difference vars ***********************
	**********************************************************************************

	preserve

		local interaction_vars ``panel''

		local mylist d_km log_area_polyg_km  potshape lpop log_TOTAL_pop_all 

		keep `mylist' id year `interaction_vars' C_banks_1981
		isid id year

		reshape wide `mylist', i(id) j(year)
		isid id

		foreach var in  `mylist' {
			gen `var'5010=`var'2010-`var'1950
		}

		label var d_km`diff' "D Shape km"
		label var log_area_polyg_km`diff' "D log area"

		
		**********************************************************************************
		******************************* Regressions  *************************************
		**********************************************************************************

		* Define consistent sample for robustness check in Appendix
		foreach k in `interaction_vars' C_banks_1981 {
			drop if `k'==.
		}
	   

		foreach k in `interaction_vars'  {

			local labeltransit: var label `k'


			**************************** Create interactions *********************************

			* Shape
			cap drop shape_x_transit
			quie gen shape_x_transit = d_km`diff' * `k'
			label var shape_x_transit "D Shape x Transit"
				
			* Shape instrument
			cap drop potshape_`k'
			quie gen potshape_`k' = potshape`diff' * `k'
				
			************************************************************
			**************************** IV ****************************
			************************************************************

			clear matrix

			ivreg2 log_TOTAL_pop_all`diff'  (  d_km`diff' shape_x_transit log_area_polyg_km`diff'  =  potshape`diff'  potshape_`k' lpop`diff' ) ,  first  cluster(id) savefirst

					
			mat define fstat =e(first)
			mat list fstat
			
			quie local APF_pop=fstat[15,3]
			quie local APF_shape=fstat[15,1]
			quie local APF_interaction=fstat[15,2]
			local APF_pop = round(`APF_pop',.01)
			local APF_pop = substr("`APF_pop'",1,strpos("`APF_pop'",".")+2)
			local APF_shape = round(`APF_shape',.01)
			local APF_shape = substr("`APF_shape'",1,strpos("`APF_shape'",".")+2)
			local APF_interaction = round(`APF_interaction',.01)
			local APF_interaction = substr("`APF_interaction'",1,strpos("`APF_interaction'",".")+2)
			qui local kptest = e(idstat)
			qui local kptest = round(`kptest',.01)
			local kptest = substr("`kptest'",1,strpos("`kptest'",".")+2)

			sum `k' if e(sample)==1
			local meanv =round(`r(mean)',0.001)
			if `r(mean)' >2 { 
				local meanv = round(`r(mean)',1)
			} 
			outreg2 using "$resultsfolder/Table9_Infrastructure.xls" ,  keep (d_km`diff' shape_x_transit log_area_polyg_km`diff' ) title(Table 9: Heterogeneous effects of infrastructure and transit) nocons nor2 ctitle (`k2', IV )  addtext(AP F stat interaction, "`APF_interaction'", AP F stat shape, "`APF_shape'" , AP F stat pop, "`APF_pop'", KP test stat, "`kptest'", "Transit variable", "`labeltransit'", "Mean interaction var", "`meanv'")   label(insert) append
				
				
			}
		
		
	restore
	}


*************************************************************
*************************************************************
*************************************************************
*************************************************************
log close 
* end

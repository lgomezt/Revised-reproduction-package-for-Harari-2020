cap restore
cap log close
clear all
set more off
set matsize 9000
version 14 

*=================================================================================================*
** Set Directory 
*=================================================================================================*
global mydir "C:\Users\User\OneDrive - Universidad de los Andes\12. Último semestre\Urban economics\Final project\Plan C. Economía\ReplicationFolder_Main"
	// change directory to directory of replication folder
cd "${mydir}" 

*=================================================================================================*
** Packages
*=================================================================================================*
*The following user-written packages are required to successfully run all scripts. 

ssc install outreg2 
ssc install ivreg2
ssc install xtivreg2 
ssc install estout 
ssc install fs 
ssc install ranktest

*=================================================================================================*
** Main
*=================================================================================================*

****** Table 1, panel A, descriptive stats panel 
do ".\Programs\Table1A_SumStatsPanel.do"

****** Table 1, panel B, descriptive stats long difference 
do ".\Programs\Table1B_SumStatsLD.do"

****** Table 2, first stage, long difference (columns 1 and 2)  
do ".\Programs\Table2_FSLD.do"

****** Table 2, first stage, panel, columns 3 and 4 
do ".\Programs\Table2_FSPanel.do"

****** Table 3, Impact of city shape on population 	
do ".\Programs\Table3_PopulationLD.do" 

****** Table 4: Impact of city shape on wages 			
do ".\Programs\Table4_WagesLD.do" 

****** Table 5: Impact of city shape on rents 			
do ".\Programs\Table5_RentsLD.do"

****** Table 6: IV impact of city shape on population, robustness to confounding trends					
do ".\Programs\Table6_IVRobustTrends.do"

****** Table 7: Falsification test with lagged outcomes, population 
do ".\Programs\Table7_Population_Placebo.do"

******* Table 8: First stage and impact of city shape on population, single instrument 	
do ".\Programs\Table8_PopulationLD_SingleIV.do"

****** Table 9: Heterogeneous effects of infrastructure and transit 
do ".\Programs\Table9_Infrastructure.do"

****** Table 10: Employment subcenters and work trips 
do ".\Programs\Table10_EmploymentCenters.do"

****** Table 11: Impact of city shape on public services 
do ".\Programs\Table11_ServicesLD.do" 

****** Table 12: Impact of city shape on slum population 
do ".\Programs\Table12_SlumsLD.do"

****** Table 13: Impact of FARs on city shape 
do ".\Programs\Table13_FARs.do"


*=================================================================================================*
** Figures
*=================================================================================================*

* Figure 1 
do ".\Programs\Figure1.do"


*=================================================================================================*
** Clean Output 
*=================================================================================================*

cd ".\Out\"
quie fs *.txt*

foreach j in  `r(files)' {
	cap erase `j'
}




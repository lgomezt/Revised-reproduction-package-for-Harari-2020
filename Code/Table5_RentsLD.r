# Table 5, Impact of city shape on rents

"""
Table 5 reports the same set of specifications for housing rents. The dependent 
variable is the 2008−2006 difference of the log yearly housing rent per square meter, 
averaged throughout all urban households in the district, from National Sample 
Survey data. The evidence presented suggest that lower rents are present in less
compact cities
"""

# Delete all variables. Is the same as "clear all" in Stata
rm(list = ls())

# Load libraries
library(haven) # To load .dta files in R
library(rstudioapi) # To get the directory of this script
library(dplyr) # To manipulate dataframes
library(tidyr) # To reshape dataframes
library(writexl) # To export dataframes to Excel
library(tibble) # Rownames to columns
library(stargazer) # Export to latex
library(lmtest) # Cluster standard errors
library(sandwich) # Robust covariance matrix estimators
library(AER) # To perform IV estimation

# Define the paths to import data, call scripts or save outputs 
path_script <- getActiveDocumentContext()$path # Automatich path :D
path_data <- gsub(x = path_script, 
                  pattern = "Revised reproduction package for Harari, 2020/Code/Table5_RentsLD.r", 
                  replacement = "ReplicationFolder_Main/Data/")
path_output <- gsub(x = path_script,
                    pattern = "Code/Table5_RentsLD.r", 
                    replacement = "Out/")

# Import CityShape_Main.dta as df
df <- read_dta(file = paste0(path_data, "CityShape_Main.dta"))

# Select relevant variables
df2 <- df %>%
  select(id, year, disconnect_km, log_area_polyg_km, r1_relev_disconnect_cls_km,
         log_projected_pop, rent_1_Mt_v0, rent_1_Mt_v1, rent_1_Mt_v2,
         per_worker_wage_Md_V0, per_worker_wage_Md_V1, per_worker_wage_Md_V2)

# Create variables in log
variables <- c("rent_1_Mt_v0", "rent_1_Mt_v1", "rent_1_Mt_v2", 
               "per_worker_wage_Md_V0", "per_worker_wage_Md_V1", 
               "per_worker_wage_Md_V2")

for (var in variables) {
  var_name <- paste("log", var, sep = "_")
  df2[, var_name] <- log(df2[, var])
}

# Reshape the data: long to wide
df2 <- df2 %>%
  pivot_wider(id_cols = id, names_from = year, 
              values_from = -all_of(c("id", "year")))

# Generate diff vars
variables <- c("disconnect_km", "log_area_polyg_km", "r1_relev_disconnect_cls_km",
               "log_projected_pop", "log_rent_1_Mt_v0", "log_rent_1_Mt_v1", "log_rent_1_Mt_v2",
               "per_worker_wage_Md_V0", "per_worker_wage_Md_V1", "per_worker_wage_Md_V2")

for (var in variables) {
  # Names of the variables
  var2008 <- paste(var, "2008", sep = "_")
  var2006 <- paste(var, "2006", sep = "_")
  vardiff <- paste(var, "diff", sep = "_")
  # Calculation
  df2[, vardiff] = df2[, var2008] - df2[, var2006]
} 

# All districts
rent0 <- ivreg(formula = log_rent_1_Mt_v0_diff ~ disconnect_km_diff + 
                  log_area_polyg_km_diff, instruments = ~ r1_relev_disconnect_cls_km_diff + 
                  log_projected_pop_diff, data = df2)
rent02 <- coeftest(rent0, vcov. = vcovHC(rent0, type = "HC1"), 
                    cluster = id)
ols0 <- ivreg(formula = log_rent_1_Mt_v0_diff ~ disconnect_km_diff + 
                log_area_polyg_km_diff, data = df2)
ols02 <- coeftest(ols0, vcov. = vcovHC(ols0, type = "HC1"), 
                  cluster = id)
n0 <- rent0$nobs

# Only districts with one city
rent1 <- ivreg(formula = log_rent_1_Mt_v1_diff ~ disconnect_km_diff + 
                  log_area_polyg_km_diff, instruments = ~ r1_relev_disconnect_cls_km_diff + 
                  log_projected_pop_diff, data = df2)
rent12 <- coeftest(rent1, vcov. = vcovHC(rent1, type = "HC1"), 
                    cluster = id)
ols1 <- ivreg(formula = log_rent_1_Mt_v1_diff ~ disconnect_km_diff + 
                log_area_polyg_km_diff, data = df2)
ols12 <- coeftest(ols1, vcov. = vcovHC(ols1, type = "HC1"), 
                  cluster = id)
n1 <- rent1$nobs

# Only top city per district
rent2 <- ivreg(formula = log_rent_1_Mt_v2_diff ~ disconnect_km_diff + 
                  log_area_polyg_km_diff, instruments = ~ r1_relev_disconnect_cls_km_diff + 
                  log_projected_pop_diff, data = df2)
rent22 <- coeftest(rent2, vcov. = vcovHC(rent2, type = "HC1"), 
                    cluster = id)
ols2 <- ivreg(formula = log_rent_1_Mt_v2_diff ~ disconnect_km_diff + 
                log_area_polyg_km_diff, data = df2)
ols22 <- coeftest(ols2, vcov. = vcovHC(ols2, type = "HC1"), 
                  cluster = id)
n2 <- rent2$nobs

# Export results
stargazer(rent02, ols02, rent12, ols12, rent22, ols22, type = "text", 
          title = "Table 5: Impact of city shape on rents",
          column.labels = c("IV \\\\ & All districts", "OLS \\\\ & All districts", "IV \\\\ & Only districts with one city", "OLS \\\\ & Only districts with one city", "IV \\\\ & Only top city per district", "OLS \\\\ & Only top city per district"),
          covariate.labels = c("D Shape, km", "D Log area"),
          dep.var.labels = c("2008-2006", "2008-2006", "2008-2006", "2008-2006", "2008-2006", "2008-2006"),
          omit = c("Constant"), 
          notes = "Robust standard errors in parentheses",
          add.lines = list(c("Observations", n0, n0, n1, n1, n2, n2),
                           c("Avg. yearly rent per m2 2006", 703, 703, 705, 705, 700, 700)), 
          out = paste0(path_output, "Table5_Rents(R).txt"))

stargazer(rent02, ols02, rent12, ols12, rent22, ols22, type = "latex", 
          title = "Table 5: Impact of city shape on rents",
          column.labels = c("IV \\\\ & All districts", "OLS \\\\ & All districts", "IV \\\\ & Only districts with one city", "OLS \\\\ & Only districts with one city", "IV \\\\ & Only top city per district", "OLS \\\\ & Only top city per district"),
          covariate.labels = c("D Shape, km", "D Log area"),
          dep.var.labels = c("2008-2006", "2008-2006", "2008-2006", "2008-2006", "2008-2006", "2008-2006"),
          omit = c("Constant"), 
          notes = "Robust standard errors in parentheses",
          add.lines = list(c("Observations", n0, n0, n1, n1, n2, n2),
                           c("Avg. yearly rent per m2 2006", 703, 703, 705, 705, 700, 700)), 
          out = paste0(path_output, "Table5_Rents(R).tex"))

# Interpretation of results
"""
the effect of disconnected shapes on rents is negative in IV and near to zero 
in OLS. According to the author, the finding of higher wages and lower rents 
in non-compact cities is consistent with a compensating differential interpretation. 

In the model developed, if compact city shape provides advantages in terms of 
quality of life or productivity, compact cities will be characterized by higher 
rents and wages that may be higher or lower depending on whether households or 
firms value compact shape the most. To the extent that households value city 
shape more than firms, they will bid wages up in compact cities. 
"""
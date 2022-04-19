# This script replicate the Table 2. It presents results from estimating the 
# first-stage equation, relating city shape and area to the geography-based 
# instrument and to projected historical population.

# THIS SCRIPT IS FOR THE FIRST STAGE FOR PANEL (COLUMNS 3 & 4)
"""
Cols. 3 and 4 report first stage F stats for the full sample of city-years 
for which shape and area are observed. This includes years for which there is no 
population data. An IV regression with population as an outcome would exclude 
those years. In order to compute the first stage F stats for this sample, 
create a temporary dependent variable that is never missing  and use it to run an 
IV regression for the sole purpose of retrieveing the F stats.
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
                  pattern = "Revised reproduction package for Harari, 2020/Code/Table2_FSPanel.r", 
                  replacement = "ReplicationFolder_Main/Data/")
path_output <- gsub(x = path_script,
                    pattern = "Code/Table2_FSPanel.r", 
                    replacement = "Out/")

# Import CityShape_Main.dta as df
df <- read_dta(file = paste0(path_data, "CityShape_Main.dta"))

# Filter for the 351 cities sampled by the author for their main analysis.
df2 = df[df["insample_IV_5010"] == 1,]

# Reshape the data: long to wide
df2 <- df2 %>%
  select(id, year, area_polyg_km, disconnect_N_km, disconnect_km,
         log_projected_pop, r1_relev_disconnect_cls_km, log_area_polyg_km, 
         log_TOTAL_pop_all, dens_core_all, TOTAL_pop_all) %>%
  pivot_wider(id_cols = id, names_from = year, 
              values_from = -all_of(c("id", "year")))

# Generate log difference vars

# List of variables to calculate the difference
variables <- c("area_polyg_km", "disconnect_N_km", "disconnect_km",
               "log_projected_pop", "r1_relev_disconnect_cls_km", 
               "log_area_polyg_km", "log_TOTAL_pop_all", "dens_core_all", 
               "TOTAL_pop_all")
var = variables[1]

for (var in variables) {
  # Names of the variables
  var2010 <- paste(var, "2010", sep = "_")
  var1950 <- paste(var, "1950", sep = "_")
  vardiff <- paste(var, "diff", sep = "_")
  # Calculation
  df2[, vardiff] = df2[, var2010] - df2[, var1950]
} 



iv <- ivreg(formula = log_TOTAL_pop_all_diff ~ disconnect_km_diff + 
              log_area_polyg_km_diff, instruments = ~ r1_relev_disconnect_cls_km_diff + 
              log_projected_pop_diff, data = df2)
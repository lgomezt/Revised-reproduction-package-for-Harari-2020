# Table 3, Impact of city shape on population.

# Table3 reports estimates of the impact of city shape on population, the main 
# outcome of interest. The author estimate long-difference equation by IV and OLS.

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
                  pattern = "Revised reproduction package for Harari, 2020/Code/Table3_PopulationLD.r", 
                  replacement = "ReplicationFolder_Main/Data/")
path_output <- gsub(x = path_script,
                    pattern = "Code/Table3_PopulationLD.r", 
                    replacement = "Out/")

# Import CityShape_Main.dta as df
df <- read_dta(file = paste0(path_data, "CityShape_Main.dta"))

# Filter for the 351 cities sampled by the author for their main analysis.
df2 = df[df["insample_IV_5010"] == 1,]


iv <- ivreg(formula = log_TOTAL_pop_all_diff ~ disconnect_km_diff + 
              log_area_polyg_km_diff, instruments = ~ r1_relev_disconnect_cls_km_diff + 
              log_projected_pop_diff, data = df2)
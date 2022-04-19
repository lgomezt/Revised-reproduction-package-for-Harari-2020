# This script replicate the Table 2. It presents results from estimating the 
# two first-stage equations, relating city shape and area to the geography-based 
# instrument and to projected historical population.

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

# Define the paths to import data, call scripts or save outputs 
path_script <- getActiveDocumentContext()$path # Automatich path :D
path_data <- gsub(x = path_script, 
                  pattern = "Revised reproduction package for Harari, 2020/Code/Table2_FSLD.r", 
                  replacement = "ReplicationFolder_Main/Data/")
path_output <- gsub(x = path_script,
                    pattern = "Code/Table2_FSLD.r", 
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

# Regression to get first stage. F and KP stat

# 1. Comment --------------------------------------------------------------
"""
The author is going to estimate the effect of the shape of the city on the 
growth of the population. However, due to the endogeneity of the shape of the 
city, she is going to instrument its regressor with some topographic data.

As IV has two main conditions to be used (exlusion restriction and relevance
condition), this part of the paper is focused to show the relevance of the 
instrument. Most of the test are based on the global significance of the first
stage of the IV (using F-Statistic) and the individual significance of the
instruments.

Here we are going to reproduce the first stage of the paper. We are focused in
calculate the Angrist-Pischke and Kleibergen-Paap F-statistics.

Angrist and Pischke propose an first-stage-statistic for the case of multiple 
endogenous variables by reformulating the estimation problem to a one-variable 
model after replacing the other endogenous variables with their reduced form 
predictions. A good explanaiton can be found here:
https://www.sciencedirect.com/science/article/pii/S0304407615001736#br000005
"""

# First stage for Shape
f1_shape <- lm(disconnect_km_diff ~ r1_relev_disconnect_cls_km_diff + 
                 log_projected_pop_diff, data = df2)
f1_shape2 <- coeftest(f1_shape, vcov. = vcovHC(f1_shape, type = "HC0"), cluster = id)


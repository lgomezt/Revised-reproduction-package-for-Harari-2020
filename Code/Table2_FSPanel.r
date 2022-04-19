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

# First stage for Shape (Panel)
temp = df2[,"disconnect_km"]*2
f1_shape <- lm(disconnect_km ~ factor(id) + factor(year) + 
                 log_projected_pop + r1_relev_disconnect_cls_km, data = df2, 
               subset = !is.na(temp))
f1_shape2 <- coeftest(f1_shape, vcov. = vcovHC(f1_shape, type = "HC1"), 
                      cluster = id)

# First stage for Area (Panel)
f1_area <- lm(log_area_polyg_km ~ factor(id) + factor(year) + 
                log_projected_pop + r1_relev_disconnect_cls_km, data = df2, 
              subset = !is.na(temp))
f1_area2 <- coeftest(f1_area, vcov. = vcovHC(f1_area, type = "HC1"), 
                     cluster = id)

stargazer(f1_shape2, f1_area2, type = "text", 
          title = "Table 2 (columns 3 and 4): First stage panel",
          column.labels = c("OLS", "OLS"),
          dep.var.labels = c("Shape, km", "Log area, km"),
          covariate.labels = c("Log projected historical population", "Potential shape, km"),
          omit = c("Constant", "id", "year"), 
          notes = "Robust standard errors in parentheses",
          add.lines = list(c("Observations", sum(!is.na(temp)), sum(!is.na(temp)))), 
          out = paste0(path_output, "Table2_Panel_FirstStage(R).txt"))

stargazer(f1_shape2, f1_area2, type = "latex", 
          title = "Table 2 (columns 3 and 4): First stage panel",
          column.labels = c("OLS", "OLS"),
          dep.var.labels = c("Shape, km", "Log area, km"),
          covariate.labels = c("Log projected historical population", "Potential shape, km"),
          omit = c("Constant", "id", "year"), 
          notes = "Robust standard errors in parentheses",
          add.lines = list(c("Observations", sum(!is.na(temp)), sum(!is.na(temp)))), 
          out = paste0(path_output, "Table2_Panel_FirstStage(R).tex"))

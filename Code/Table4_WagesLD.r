# Table 4, Impact of city shape on wages 

# In Table 4 reports the IV and OLS relationship between average wages and 
# city shape. In conclusion, non-compact cities are associated with higher wages.


"""
Data are available at the district level but the matching between districts 
and cities is not one to one. The author uses three samples: 
- Any city that can be matched (columns 1 and 2); 
- Cities for which there is a one-to-one mapping with a district (columns 3 and 4); 
- Top city per district (columns 5 and 6).
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
                  pattern = "Revised reproduction package for Harari, 2020/Code/Table4_WagesLD.r", 
                  replacement = "ReplicationFolder_Main/Data/")
path_output <- gsub(x = path_script,
                    pattern = "Code/Table4_WagesLD.r", 
                    replacement = "Out/")

# Import CityShape_Main.dta as df
df <- read_dta(file = paste0(path_data, "CityShape_Main.dta"))

# The ASI data are available at the district level. Keep relevant observations.
df2 = df[df["insample_FS_FullPanel"] == 1,]

# There are not information for the wages in 1992, therefore, the author uses
# the closest year for which they are observed (1990)

# Select relevant variables
df2 <- df2 %>%
  select(id, year, disconnect_km, log_area_polyg_km, r1_relev_disconnect_cls_km,
         log_projected_pop, per_worker_wage_Md_V0, per_worker_wage_Md_V1,
         per_worker_wage_Md_V2, log_TOTAL_pop_all)

# Create variables in log
variables <- c("per_worker_wage_Md_V0", "per_worker_wage_Md_V1", 
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
variables <- c("disconnect_km", "log_projected_pop", "r1_relev_disconnect_cls_km", 
               "log_area_polyg_km")

for (var in variables) {
  # Names of the variables
  var2010 <- paste(var, "2010", sep = "_")
  var1992 <- paste(var, "1992", sep = "_")
  vardiff <- paste(var, "diff", sep = "_")
  # Calculation
  df2[, vardiff] = df2[, var2010] - df2[, var1992]
} 

variables <- c("log_per_worker_wage_Md_V0", "log_per_worker_wage_Md_V1", "log_per_worker_wage_Md_V2")

for (var in variables) {
  # Names of the variables
  var2010 <- paste(var, "2010", sep = "_")
  var1992 <- paste(var, "1992", sep = "_")
  vardiff <- paste(var, "diff", sep = "_")
  # Calculation
  df2[, vardiff] = df2[, var2010] - df2[, var1992]
} 

# All districts
wages0 <- ivreg(formula = log_per_worker_wage_Md_V0_diff ~ disconnect_km_diff + 
                  log_area_polyg_km_diff, instruments = ~ r1_relev_disconnect_cls_km_diff + 
                  log_projected_pop_diff, data = df2)
wages02 <- coeftest(wages0, vcov. = vcovHC(wages0, type = "HC1"), 
                    cluster = id)
ols0 <- ivreg(formula = log_per_worker_wage_Md_V0_diff ~ disconnect_km_diff + 
                log_area_polyg_km_diff, data = df2)
ols02 <- coeftest(ols0, vcov. = vcovHC(ols0, type = "HC1"), 
                  cluster = id)
n0 <- wages0$nobs

# Only districts with one city
wages1 <- ivreg(formula = log_per_worker_wage_Md_V1_diff ~ disconnect_km_diff + 
                  log_area_polyg_km_diff, instruments = ~ r1_relev_disconnect_cls_km_diff + 
                  log_projected_pop_diff, data = df2)
wages12 <- coeftest(wages1, vcov. = vcovHC(wages1, type = "HC1"), 
                    cluster = id)
ols1 <- ivreg(formula = log_per_worker_wage_Md_V1_diff ~ disconnect_km_diff + 
                log_area_polyg_km_diff, data = df2)
ols12 <- coeftest(ols1, vcov. = vcovHC(ols1, type = "HC1"), 
                  cluster = id)
n1 <- wages1$nobs

# Only top city per district
wages2 <- ivreg(formula = log_per_worker_wage_Md_V2_diff ~ disconnect_km_diff + 
                  log_area_polyg_km_diff, instruments = ~ r1_relev_disconnect_cls_km_diff + 
                  log_projected_pop_diff, data = df2)
wages22 <- coeftest(wages2, vcov. = vcovHC(wages2, type = "HC1"), 
                    cluster = id)
ols2 <- ivreg(formula = log_per_worker_wage_Md_V2_diff ~ disconnect_km_diff + 
                log_area_polyg_km_diff, data = df2)
ols22 <- coeftest(ols2, vcov. = vcovHC(ols2, type = "HC1"), 
                  cluster = id)
n2 <- wages2$nobs

# Export results
stargazer(wages02, ols02, wages12, ols12, wages22, ols22, type = "text", 
          title = "Table 4: Impact of city shape on wages",
          column.labels = c("IV", "OLS", "IV", "OLS", "IV", "OLS"),
          covariate.labels = c("D Shape, km", "D Log area"),
          dep.var.labels = c("2010-1992", "2010-1992", "2010-1992", "2010-1992", "2010-1992", "2010-1992"),
          omit = c("Constant"), 
          notes = "Robust standard errors in parentheses",
          add.lines = list(c("Observations", n0, n0, n1, n1, n2, n2),
                           c("Avg. yearly wage 1992", rep(72, 6)),
                           c("Avg. yearly wage 2010", 187, 187, 193, 193, 187, 187)), 
          out = paste0(path_output, "Table4_Wages(R).txt"))

stargazer(wages02, ols02, wages12, ols12, wages22, ols22, type = "latex", 
          title = "Table 4: Impact of city shape on wages",
          column.labels = c("IV", "OLS", "IV", "OLS", "IV", "OLS"),
          covariate.labels = c("D Shape, km", "D Log area"),
          dep.var.labels = c("2010-1992", "2010-1992", "2010-1992", "2010-1992", "2010-1992", "2010-1992"),
          omit = c("Constant"), 
          notes = "Robust standard errors in parentheses",
          add.lines = list(c("Observations", n0, n0, n1, n1, n2, n2),
                           c("Avg. yearly wage 1992", rep(72, 6)),
                           c("Avg. yearly wage 2010", 187, 187, 193, 193, 187, 187)), 
          out = paste0(path_output, "Table4_Wages(R).tex"))

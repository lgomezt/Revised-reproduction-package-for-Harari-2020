# Table 6, IV impact of city shape on population, robustness to confounding trends

"""
Table 6, shows that the IV estimates for the impact of city shape on population 
are robust to controlling for diverse characteristics. She reports the same 
IV specification as in Table 3, column 1, augmented with time-invariant geography 
controls. All point estimates are very similar to the baseline.

Before, we talked about the assumptions needed to perform an IV estimation. One 
of those assumptions is the exclusion restriction. That means that the instrument
Z affects the outcome variable Y only through X (Z does not have a direct influence 
on Y).

In this case, the exclusion restriction for the shape instrument requires that 
potential shape only affects the outcomes of interest though the constraints 
that it posits to urban form. One of the major identification threats is that 
the instrument may be correlated with geographic characteristics. For example, 
coasts and mountains may also make cities intrinsically more or less attractive 
for households and/or firms.

These direct effects of geography could bias the IV results in different directions. 
If the instrument picked up the effect of coasts and the latter were landscape 
amenities, the estimated effects of bad shape on population would be biased towards 
positive values. Conversely, if potential shape were less compact in areas with 
particularly deep bedrock, the IV impacts of shape on population could be biased 
towards more negative values, as they would be mediated by higher construction 
costs in those cities
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
                  pattern = "Revised reproduction package for Harari, 2020/Code/Table6_IVRobustTrends.r", 
                  replacement = "ReplicationFolder_Main/Data/")
path_output <- gsub(x = path_script,
                    pattern = "Code/Table6_IVRobustTrends.r", 
                    replacement = "Out/")

# Import CityShape_Main.dta as df
df <- read_dta(file = paste0(path_data, "CityShape_Main.dta"))

# Filter for the 351 cities sampled by the author for their main analysis.
df2 = df[df["insample_IV_5010"] == 1,]

# Create the variable of elevation 100 m
df2[, "elevation"] = df2[, "elevation_m"]/100

# Reshape the data: long to wide
df2 <- df2 %>%
  pivot_wider(id_cols = id, names_from = year, 
              values_from = -all_of(c("id", "year")))

# Generate diff vars
variables <- names(select(head(df), contains("_")))

for (var in variables) {
  # Names of the variables
  var2010 <- paste(var, "2010", sep = "_")
  var1950 <- paste(var, "1950", sep = "_")
  vardiff <- paste(var, "diff", sep = "_")
  # Calculation
  df2[, vardiff] = df2[, var2010] - df2[, var1950]
} 

# Estimates
# Elevation
elevation <- ivreg(formula = log_TOTAL_pop_all_diff ~ disconnect_km_diff + 
              log_area_polyg_km_diff + control, 
            instruments = ~ r1_relev_disconnect_cls_km_diff + 
              log_projected_pop_diff + control, 
            data = rename(df2, "control" = "elevation_1950"))
elevation2 <- coeftest(elevation, vcov. = vcovHC(elevation, type = "HC1"), 
                cluster = id)

# Distance from coast
coast_dist_km <- ivreg(formula = log_TOTAL_pop_all_diff ~ disconnect_km_diff + 
                     log_area_polyg_km_diff + control, 
                   instruments = ~ r1_relev_disconnect_cls_km_diff + 
                     log_projected_pop_diff + control, 
                   data = rename(df2, "control" = "coast_dist_km_1950"))
coast_dist_km2 <- coeftest(coast_dist_km, vcov. = vcovHC(coast_dist_km, type = "HC1"), 
                       cluster = id)

# Distance from river/lake
dist_riverorlake <- ivreg(formula = log_TOTAL_pop_all_diff ~ disconnect_km_diff + 
                         log_area_polyg_km_diff + control, 
                       instruments = ~ r1_relev_disconnect_cls_km_diff + 
                         log_projected_pop_diff + control, 
                       data = rename(df2, "control" = "dist_riverorlake_1950"))
dist_riverorlake2 <- coeftest(dist_riverorlake, vcov. = vcovHC(dist_riverorlake, type = "HC1"), 
                           cluster = id)

# Distance from mineral deposit
distance_mineral_km <- ivreg(formula = log_TOTAL_pop_all_diff ~ disconnect_km_diff + 
                            log_area_polyg_km_diff + control, 
                          instruments = ~ r1_relev_disconnect_cls_km_diff + 
                            log_projected_pop_diff + control, 
                          data = rename(df2, "control" = "distance_mineral_km_1950"))
distance_mineral_km2 <- coeftest(distance_mineral_km, vcov. = vcovHC(distance_mineral_km, type = "HC1"), 
                              cluster = id)

# Ruggedness
ROUGH <- ivreg(formula = log_TOTAL_pop_all_diff ~ disconnect_km_diff + 
                            log_area_polyg_km_diff + control, 
                          instruments = ~ r1_relev_disconnect_cls_km_diff + 
                            log_projected_pop_diff + control, 
                          data = rename(df2, "control" = "ROUGH_1950"))
ROUGH2 <- coeftest(dist_riverorlake, vcov. = vcovHC(ROUGH, type = "HC1"), 
                              cluster = id)

# Bedrock depth
bedrockdepth <- ivreg(formula = log_TOTAL_pop_all_diff ~ disconnect_km_diff + 
                            log_area_polyg_km_diff + control, 
                          instruments = ~ r1_relev_disconnect_cls_km_diff + 
                            log_projected_pop_diff + control, 
                          data = rename(df2, "control" = "bedrockdepth_1950"))
bedrockdepth2 <- coeftest(bedrockdepth, vcov. = vcovHC(bedrockdepth, type = "HC1"), 
                              cluster = id)

# Crop suitability
average_suit <- ivreg(formula = log_TOTAL_pop_all_diff ~ disconnect_km_diff + 
                            log_area_polyg_km_diff + control, 
                          instruments = ~ r1_relev_disconnect_cls_km_diff + 
                            log_projected_pop_diff + control, 
                          data = rename(df2, "control" = "average_suit_1950"))
average_suit2 <- coeftest(average_suit, vcov. = vcovHC(average_suit, type = "HC1"), 
                              cluster = id)

# Export results
stargazer(elevation2, coast_dist_km2, dist_riverorlake2, distance_mineral_km2, ROUGH2, bedrockdepth2, average_suit2, 
          type = "text", 
          title = "Table 6: IV impact of city shape on population robustness to confounding trends",
          covariate.labels = c("D Shape, km", "D Log area", "Control"),
          column.labels = rep("IV", 7),
          dep.var.labels = c("Log Pop D2010-1950"),
          omit = c("Constant"), 
          notes = "Robust standard errors in parentheses",
          add.lines = list(c("Observations", rep(351, 7)),
                           c("Control", "Elevation 100 m", "Distance from the coast", "Distance from nearest river or lake", "Distance from nearest mineral deposit", "Ruggedness", "Bedrock depth", "Crop suitability")), 
          out = paste0(path_output, "Table6_IVRobustTrends(R).txt"), align = T)

stargazer(elevation2, coast_dist_km2, dist_riverorlake2, distance_mineral_km2, ROUGH2, bedrockdepth2, average_suit2, 
          type = "latex", 
          title = "Table 6: IV impact of city shape on population robustness to confounding trends",
          covariate.labels = c("D Shape, km", "D Log area", "Control"),
          column.labels = rep("IV", 7),
          dep.var.labels = c("Log Pop D2010-1950"),
          omit = c("Constant"), 
          notes = "Robust standard errors in parentheses",
          add.lines = list(c("Observations", rep(351, 7)),
                           c("Control", "Elevation 100 m", "Distance //// & from the coast", "Distance from //// & nearest river or lake", "Distance from //// & nearest mineral deposit", "Ruggedness", "Bedrock //// & depth", "Crop //// & suitability")), 
          out = paste0(path_output, "Table6_IVRobustTrends(R).tex"), align = T)

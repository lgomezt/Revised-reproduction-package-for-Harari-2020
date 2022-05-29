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

for (var in variables) {
  # Names of the variables
  var2010 <- paste(var, "2010", sep = "_")
  var1950 <- paste(var, "1950", sep = "_")
  vardiff <- paste(var, "diff", sep = "_")
  # Calculation
  df2[, vardiff] = df2[, var2010] - df2[, var1950]
} 

# Why the author are using IV? What are the sources of endogeneity?
"""
The main concern in estimating the effect of shape of a city in the size of its
population is the endogeneity of urban geometry. The observed spatial structure 
of a city at a given point in time is the result of the interaction of exogenous 
factors, such as geography, and factors endogenous to population, such as the 
city’s growth rate and policy choices. For example, master plans, land use 
regulations and investments in road infrastructure affects city shape. This 
induces a simultaneous correlation between city shape and city size. 

The sign of the OLS bias will be ambiguous, as the selection effects induced by 
the endogenous determinants of city shape operate in different directions. For
example, areas with stronger state capacity tend to have better urban planning 
and enforcement of master plans, and may be more compact. At the same time, cities 
with stronger institutional capacity and well-functioning local governments also 
tend to be more successful and faster-growing cities. This may result in fast-growing 
cities having better shapes, for reasons unrelated to the value of compactness.
This would tend to generate a negative correlation between non-compact shape and 
population.

On the other hand, population growth may make cities less compact. As cities grow, 
they tend to deteriorate in shape. A city experiencing faster population growth 
may be harder to manage from an urban planning perspective, resulting in more 
chaotic development. Large cities may have more fragmented governance as they 
stretch over multiple administrative units. These effects would tend to generate a 
positive correlation between non-compact shape and population.
"""

# IV Estimates
iv <- ivreg(formula = log_TOTAL_pop_all_diff ~ disconnect_km_diff + 
              log_area_polyg_km_diff, instruments = ~ r1_relev_disconnect_cls_km_diff + 
              log_projected_pop_diff, data = df2)
iv2 <- coeftest(iv, vcov. = vcovHC(iv, type = "HC0"), 
                      cluster = id)

# Interpretation of the results
"""
The betas of the IV regression show that as city becomes less compact (holding
area constant), the population growth declines. An increase in the average distance 
between points of 360 meters (one-standar deviation increase), holding constant 
city area, is associated with a 3.5 percent decline in population.
"""

# OLS Estimates
ols <- lm(formula = log_TOTAL_pop_all_diff ~ disconnect_km_diff + 
             log_area_polyg_km_diff, data = df2)
ols2 <- coeftest(ols, vcov. = vcovHC(ols, type = "HC0"), 
                 cluster = id)

# Interpretation of the results
"""
In equilibrium, faster growing cities are cities that grow into more disconnected
shapes. A 1% increase in population is associated with a deterioration in shape of
450 meters. Potential channels of this effect are more difficult urban planning or 
governance, urban growth occurring along transit corridors, and the tendency of
cities to expand into less favorable terrain.
"""

# Explanation of instruments
"""
The author instrument actual shape of the city at given time with the potential 
shape the city can have. The potential shape is estimated using the geographic 
constraints that each city faces at that stage of its predicted growth.

More concretly, she used the largest contiguous patch of developable land. It is
land not occupied by a water body nor by steep terrain, within a given predicted
radius around each city. This contiguous patch is denoted as 'potential footprint'.

She computes the shape indicator of the potential footprint and use it as an 
instrument for the shape of the actual urban footprint. What gives time 
variation to this instrument is the fact that the predicted radius is time-varying,
and  expands over time based on a mechanical model for city expansion. 

The reason why she uses predicted and not the actual growth is due endogeneity. 
However, the author do not report the results of her predictions in the paper.
"""

# Export results
stargazer(iv2, ols2, type = "text", 
          title = "Table 3: Impact of city shape on population",
          column.labels = c("IV", "OLS"),
          covariate.labels = c("D Shape, km", "D Log area"),
          dep.var.labels = c("Log Pop D2010-1950", "Log Pop D2010-1950"),
          omit = c("Constant"), 
          notes = "Robust standard errors in parentheses",
          add.lines = list(c("Observations", ols$n, ols$n)), 
          out = paste0(path_output, "Table3_ShapeOnPopulation(R).txt"))

stargazer(iv2, ols2, type = "latex", 
          title = "Table 3: Impact of city shape on population",
          column.labels = c("IV", "OLS"),
          covariate.labels = c("D Shape, km", "D Log area"),
          dep.var.labels = c("Log Pop D2010-1950", "Log Pop D2010-1950"),
          omit = c("Constant"), 
          notes = "Robust standard errors in parentheses",
          add.lines = list(c("Observations", ols$n, ols$n)),
          out = paste0(path_output, "Table3_ShapeOnPopulation(R).tex"))

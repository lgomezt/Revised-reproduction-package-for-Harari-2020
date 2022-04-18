# This script replicate the Table 1, Panel A, Descriptive statistics, Panel 
# Panel A reports descriptive statistics from the 351 cities in the main 
# estimation sample, in all years for which  data is available. City area, 
# shape, and potential shape are observable in years 1950 and 1992–2010. City 
# population is available for census years 1951, 1991, 2001, and 2011

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

# Define the paths to import data, call scripts or save outputs 
path_script <- getActiveDocumentContext()$path
path_data <- gsub(x = path_script, 
                  pattern = "Revised reproduction package for Harari, 2020/Code/Table1A_SumStatsPanel.r", 
                  replacement = "ReplicationFolder_Main/Data/")
path_output <- gsub(x = path_script,
                    pattern = "Code/Table1A_SumStatsPanel.r", 
                    replacement = "Out/")

# Import CityShape_Main.dta as df
df <- read_dta(file = paste0(path_data, "CityShape_Main.dta"))

# Panel A. 1950, 1992–2010
# We show the statistic for the 351 cities sampled by the author for their
# main analysis.
df2 = df[df["insample_IV_5010"] == 1,]

# We create a function to count the number of rows that are not NA
not.na = function(x, na.rm = T) {sum(!is.na(x), na.rm)}

# Calculate summary statistics for area_polyg_km, disconnect_km, 
# r1_relev_disconnect_cls_km
stats <- df2 %>%
  select(area_polyg_km, disconnect_km, r1_relev_disconnect_cls_km) %>%
  summarise_all(.funs = c("not.na", "mean", "median", "sd", "min", "max"), na.rm = T) %>%
  pivot_longer(everything()) %>% 
  extract(name, into = c("variable", "stats"), "(.*)_([^_]+)$") %>%
  pivot_wider(names_from = "variable")

# Calculate summary statistics for TOTAL_pop_all.
# We only take in account those cities that are in our sample. Therefore, we drop
# the cities that doesn't have information for log_area_polyg_km, id, year, 
# disconnect_km

stats2 <- df2 %>%
  drop_na(log_TOTAL_pop_all, log_area_polyg_km, id, year, disconnect_km) %>%
  select(TOTAL_pop_all) %>%
  summarise_all(.funs = c("not.na", "mean", "median", "sd", "min", "max"), na.rm = T)

# Append stats and stats2 and prettify to create the latex output
stats2$variable = "City population"
stats <- data.frame(t(stats))
stats <- rownames_to_column(stats, "variable")
stats[1, 1] <- "variable"
names(stats) <- stats[1,]
stats <- stats[-1,]
stats3 <- rbind(stats, stats2)

# Rename columns and variables
names(stats3) <- c("Variables", "Observations", "Mean", "Median", "SD", "Min", "Max")
stats3$Variables <- c("Area, km2", "Shape, km", "Potential shape, km", 
                      "City population")

# Round decimals and put every number pretty
stats3$Observations <- as.integer(stats3$Observations)
stats3[1:3, c("Mean", "Median", "SD", "Min", "Max")] <- apply(
  stats3[1:3, c("Mean", "Median", "SD", "Min", "Max")], 2, 
      function(x) round(as.numeric(x), 2))
stats3[4, c("Mean", "Median", "SD", "Min", "Max")] <- apply(
  stats3[4, c("Mean", "Median", "SD", "Min", "Max")], 2, 
  function(x) round(as.numeric(x), 0))
stats3[1, 7] <- as.integer(stats3[1, 7])

# Produce Latex output
stargazer(stats3, summary = F, rownames = F, type = "latex",
          out = paste0(path_output, "Table1A(R).tex"))

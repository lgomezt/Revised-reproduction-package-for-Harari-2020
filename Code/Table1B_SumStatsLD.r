# This script replicate the Table 1, Panel B. Long difference
# Panel B reports variable averages for the 351 cities in the main estimation 
# sample for years 1950, 2010, and for the long difference 2010−1950. For city 
# population, 1950 and  2010 correspond to census years 1951 and 2011, 
# respectively.

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
path_script <- getActiveDocumentContext()$path # Automatich path :D
path_data <- gsub(x = path_script, 
                  pattern = "Revised reproduction package for Harari, 2020/Code/Table1B_SumStatsLD.r", 
                  replacement = "ReplicationFolder_Main/Data/")
path_output <- gsub(x = path_script,
                    pattern = "Code/Table1B_SumStatsLD.r", 
                    replacement = "Out/")

# Import CityShape_Main.dta as df
df <- read_dta(file = paste0(path_data, "CityShape_Main.dta"))

# Panel B. 
# We show the statistic for the 351 cities sampled by the author for their
# main analysis.
df2 = df[df["insample_IV_5010"] == 1,]

# We are interested in show de difference between 2010 and 1950. Therefore, we
# are going to concentrate in the mean of each year
stats <- df2 %>%
  group_by(year) %>%
  summarise(across(c(area_polyg_km, disconnect_km, r1_relev_disconnect_cls_km, 
                     TOTAL_pop_all), list("mean" = ~mean(.x, na.rm = T),
                                          "sd" = ~sd(.x, na.rm = T)))) %>%
  filter(year %in% c(1950, 2010)) %>%
  pivot_longer(-year) %>% 
  extract(name, into = c("variable", "stats"), "(.*)_([^_]+)$") %>%
  pivot_wider(names_from = "variable")

# Now, we are going to calculate the difference between 2010 and 1950
stats2 <- df2 %>%
  group_by(id) %>%
  filter(year %in% c(1950, 2010)) %>%
  summarise(across(c(area_polyg_km, disconnect_km, r1_relev_disconnect_cls_km, 
                     TOTAL_pop_all), list("diff" = ~diff(.x, na.rm = T)))) %>%
  summarise(across(c(area_polyg_km_diff, disconnect_km_diff, r1_relev_disconnect_cls_km_diff, 
                     TOTAL_pop_all_diff), list("mean" = ~mean(.x, na.rm = T),
                                          "sd" = ~sd(.x, na.rm = T)))) %>%
  pivot_longer(everything()) %>% 
  extract(name, into = c("variable", "stats"), "(.*)_([^_]+)$") %>%
  pivot_wider(names_from = "variable")

# Append two tables 
names(stats2) <- names(stats)[-1]
stats2$year <- "2010-1950"
stats3 <- rbind(stats, stats2)

# Done! Let's arrange the format and export to Latex
stats3 <- data.frame(t(stats3))
stats3 <- rownames_to_column(stats3, "variable")
stats3[1, 1] <- "variable"
names(stats3) <- paste(stats3[1,], stats3[2,], sep = "_")
stats3 <- stats3[-c(1:2),]
stats3$variable_stats <- c("Area, km2", "Shape, km", "Potential shape, km", 
                           "City population")
stats3 <- stats3 %>%
  pivot_longer(cols = -variable_stats, names_sep = "_", 
               names_to = c("year", "stat")) %>%
  pivot_wider(names_from = "year")

# Round decimals and put every number pretty
stats3[, 3:5] <- apply(stats3[, 3:5], 2, function(x) round(as.numeric(x), 2))
stats3[7:8, 3:5] <- apply(stats3[7:8, 3:5], 2, function(x) as.integer(x))
names(stats3)[1:2] <- c("Variables", "Statistic")

# Produce Latex output
stargazer(stats3, summary = F, rownames = F, type = "latex",
          out = paste0(path_output, "Table1B(R).tex"))

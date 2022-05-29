# This script replicate the table that accompanies the figure 1. The code to
# replicate the figure 1 is not provided by the authors

# Delete all variables. Is the same as "clear all" in Stata
rm(list = ls())

# Load libraries
library(haven) # To load .dta files in R
library(rstudioapi) # To get the directory of this script
library(dplyr) # To manipulate dataframes
library(tidyr) # To reshape dataframes
library(writexl) # To export dataframes to Excel

# Define the paths to import data, call scripts or save outputs 
path_script <- getActiveDocumentContext()$path # Automatic path :D
path_data <- gsub(x = path_script, 
                  pattern = "Revised reproduction package for Harari, 2020/Code/Figure1.r", 
                  replacement = "ReplicationFolder_Main/Data/")
path_output <- gsub(x = path_script,
                    pattern = "Code/Figure1.r", 
                    replacement = "Out/")

# Import CityShape_Main.dta as df
df <- read_dta(file = paste0(path_data, "CityShape_Main.dta"))

# Filter the data to select only Kolkata (id = 457) and Bangalore (id = 150)
# for the year 2005
df <- df %>%
  filter(year == 2005) %>%
  filter(id == 457 | id == 150) %>%
  select(spin_km, range_km, remoteness_km, disconnect_km, spin_N_km,
         range_N_km, remoteness_N_km, disconnect_N_km, area_polyg_km, id)

# Create new variable called NoN_EACradius that represents the radious of a circle
# If Area = pi * radio^2 => radio = (Area/pi)^0.5
df$NoN_EACradius = sqrt(df$area_polyg_km/pi) # pi~ 3.141593 
df$Norm_EACradius = df$NoN_EACradius # 

# The autor puts the suffix NoN or Norm in the name of the variables to declare
# that it is Not Normalized (NoN) or it is Normalized (Norm). Therefore, she 
# changes the name of some variables:
names(df) <- c("NoN_spin", "NoN_range", "NoN_remoteness", "NoN_disconnect",
               "Norm_spin", "Norm_range", "Norm_remoteness", "Norm_disconnect",
               "area_polyg_km", "id", "NoN_EACradius", "Norm_EACradius")

# Drop area_polyg_km
df <- select(df, -area_polyg_km)

# Reshape from wide to long
df <- df %>%
  pivot_longer(-id, names_to = c("type", "Shape metric"), names_sep = "_") %>%
  pivot_wider(id_cols = c("id", "Shape metric"), names_from = "type")

# Put the city name and drop id
df <- df %>%
  mutate(city = ifelse(id == 150, "B", "K")) %>%
  select(-id)

# Reshape from long to wide
df <- df %>%
  pivot_wider(id_cols = "Shape metric", names_from = "city",
              values_from = c("NoN", "Norm"))

# Create the variable NoN_rescale_B as Norm_B * K_area 
K_area = df$NoN_K[df$`Shape metric` == "EACradius"]
df$NoN_rescale_B = df$Norm_B * K_area

# Create the variables Adj_Diff and NoAdj_Diff 
df$Adj_Diff = df$NoN_K - df$NoN_rescale_B
df$NoAdj_Diff = df$NoN_K - df$NoN_B

# Drop EACradius metric
df <- df[df$`Shape metric` != "EACradius",]

# Change the names of the shape metrics and the columns to export the file
df <- select(df, c(`Shape metric`, NoN_K, Norm_K, NoN_rescale_B, Norm_B))
df$`Shape metric` <- c("3. Spin, km2", "4. Range, km", "2. Remoteness, km", 
                       "1. Disconnection, km")
names(df) <-  c("Shape metric", "NonNormMetric_Kolkata", "Normalized_Kolkata",
                "NonNormMetric_Bangalore", "Normalized_Bangalore")
df <- arrange(df, `Shape metric`)

# Export the data to Excel
write_xlsx(x = df, path = paste0(path_output, "Figure1(R).xlsx"))

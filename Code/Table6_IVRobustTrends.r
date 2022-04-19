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


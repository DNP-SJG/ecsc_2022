# ------------------------------------------------------------------------------------------------ #
# OUT TABLES REPORTING - ECSC 2022
#  2023/07/27
# ------------------------------------------------------------------------------------------------ #

library(haven) # Stata file loading/saving
library(openxlsx)  # Excel file loading/saving
library(Microsoft365R) # One drive login
library(AzureGraph) # One drive read
library(dplyr) # Collapsing and data management
#library(plyr)
library(tidyr) # Data management
library(ggplot2) #Plots
library(stats)
library(stringr) # leading zeros and other string work

# 00. LOADS  AND MERGES CHAPTERS -------------------------------------------------------------------

# od <- get_business_onedrive()
# od$list_files()

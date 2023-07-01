# ------------------------------------------------------------------------------------------------ #
# VARIABLE LABELING OVER OUT TABLES - ECSC 2022
#  2023/06/23
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

# 00. LOADS OUT TABLES  ----------------------------------------------------------------------------

# od <- get_business_onedrive()
# od$list_files()

## 00.01 Loads tables ------------------------------------------------------------------------------

# Loads consolidated individuals, problems and institutions table - - - - - - - - - - - - - - - - - 
#
# Individuals table
#
gd <- c(
  "https://drive.google.com/file/d/1kiSAB_gY-4mXOt1EcI2x4sDTB-FG6kxl/view", #Individuals
  "https://drive.google.com/file/d/1JxTGqtd-oSfQIkbdfoQrzmMn0aMcewAj/view?usp=sharing", #Problems
  "https://drive.google.com/file/d/1E6BV8_8cZm1nNUQbxg6IvGulYBbqcB3u/view?usp=sharing" #Institutions
    )

files_f <- function(x){
    usethis::create_download_url(x) |>
    url() |>
    readRDS() |>
    tibble::as_tibble()
  }

dt_list <- lapply(gd,files_f);rm(gd)
names(dt_list) <- c('dt_p','dt_pj_rt_p','dt_allvisited_inst')

dt_p <- dt_list[['dt_p']]
dt_pj_rt_p <- dt_list[['dt_pj_rt_p']]
dt_allvisited_inst <- dt_list[['dt_allvisited_inst']]




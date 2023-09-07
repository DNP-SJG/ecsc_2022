# ------------------------------------------------------------------------------------------------ #
# OUT TABLES ROUTES - ECSC 2022
#  2023/09/07
# ------------------------------------------------------------------------------------------------ #

options(scipen=999)

lib <- c('tidyr','plyr', 'ggplot2','viridis','dplyr',
         'forcats','hrbrthemes','data.table','curl',
         'readxl','foreign','ggalt','viridis','Hmisc',
         'forcats','tidyverse')

lapply(lib, library, character.only = TRUE);rm(lib)

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
library(rstatix) # summary stats
library(tidyverse)
library(ggtext)
library(showtext)

# 00.01 LOADS  AND MERGES CHAPTERS -----------------------------------------------------------------

## Loads tables ------------------------------------------------------------------------------------

# Loads consolidated individuals, problems and institutions table - - - - - - - - - - - - - - - - -
#
# Individuals table
#
gd <- c(
  # No labeled tables
  "https://drive.google.com/file/d/1kiSAB_gY-4mXOt1EcI2x4sDTB-FG6kxl/view", #Individuals
  "https://drive.google.com/file/d/1JxTGqtd-oSfQIkbdfoQrzmMn0aMcewAj/view", #Problems
  "https://drive.google.com/file/d/1E6BV8_8cZm1nNUQbxg6IvGulYBbqcB3u/view", #Institutions
  # Labeled tables
  "https://drive.google.com/file/d/1OWQYfSHaw1jkxljqXSQExOIhbUJgiwtv/view", #Problems
  "https://drive.google.com/file/d/197V-R5Dkma-HDQzoCrOSXmY4aMYXrLjO/view", #Individuals problems 
  "https://drive.google.com/file/d/12GNSFbA0tAUVvyN7NGZDzFcU6ZBoBsfQ/view" #Problems individuals 
)

files_f <- function(x){
  usethis::create_download_url(x) |>
    url() |>
    readRDS() |>
    tibble::as_tibble()
}

dt_list <- lapply(gd,files_f);rm(gd)
names(dt_list) <- c('dt_p','dt_pj_rt_p','dt_allvisited_inst','dt_pj_rt_pl','dt_pl','dt_pj_rt_plv')

dt_pl <- as.data.frame(dt_list[['dt_pl']])
dt_pj_rt_pl <- dt_list[['dt_pj_rt_plv']]

dt_pj_rt_pl|> group_by(cc) |> summarise(fex = sum(FEX_Cx))
dt_pj_rt_pl|> group_by(caract) |> summarise(fex = sum(FEX_Cx))
dt_pj_rt_pl|> group_by(full_prob) |> summarise(fex = sum(FEX_Cx))

table (dt_pj_rt_pl$full_prob, dt_pj_rt_pl$caract)

dt  <-  dt_pj_rt_pl[dt_pj_rt_pl$full_prob == 1,]
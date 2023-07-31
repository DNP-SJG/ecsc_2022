# ------------------------------------------------------------------------------------------------ #
# OUT TABLES REPORTING - ECSC 2022
#  2023/07/31
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

## 00.01 Loads tables ------------------------------------------------------------------------------

# Loads consolidated individuals, problems and institutions table - - - - - - - - - - - - - - - - -
#
# Individuals table
#
gd <- c(
  # No label tables
  "https://drive.google.com/file/d/1kiSAB_gY-4mXOt1EcI2x4sDTB-FG6kxl/view", #Individuals
  "https://drive.google.com/file/d/1JxTGqtd-oSfQIkbdfoQrzmMn0aMcewAj/view", #Problems
  "https://drive.google.com/file/d/1E6BV8_8cZm1nNUQbxg6IvGulYBbqcB3u/view", #Institutions
  # Label tables
  "https://drive.google.com/file/d/1OWQYfSHaw1jkxljqXSQExOIhbUJgiwtv/view",  #Problems
  "https://drive.google.com/file/d/197V-R5Dkma-HDQzoCrOSXmY4aMYXrLjO/view"   #Individuals problems 
  )

files_f <- function(x){
  usethis::create_download_url(x) |>
    url() |>
    readRDS() |>
    tibble::as_tibble()
}

dt_list <- lapply(gd,files_f);rm(gd)
names(dt_list) <- c('dt_p','dt_pj_rt_p','dt_allvisited_inst','dt_pj_rt_pl','dt_pl')

# 00. DEFINES TOTAL DECLARATION UNIVERSE -----------------------------------------------------------
#
dt_pl <- dt_list[['dt_pl']]
dt_pj_rt_pl <- dt_list[['dt_pj_rt_pl']]

## Creates justiciable problem count and id. Checks total against total declaration table
## 
sum(dt_pl$FEX_C)
table(dt_pl$count); table(is.na(dt_pl$count))

dt_pl$count[is.na(dt_pl$count) == TRUE] <- 0
table(dt_pl$count)

dt_pl$jp <- dt_pl$count/dt_pl$count
table(dt_pl$jp); table(!duplicated(dt_pj_rt_pl$keyp))
dt_pl$jp[is.na(dt_pl$jp) == TRUE] <- 0

## Creates 18 and above indicator
## 
dt_pl$a18 <- 0
dt_pl$a18[dt_pl$P5785 > 17] <- 1
table(dt_pl$a18, dt_pl$jp)

# 01. CLASS, AGE MARGIN, SEX, DECLARATION ----------------------------------------------------------
# Sandkey

library(ggsankey)
library(tidyverse)
library(ggalluvial)

dt <- dt_pl |> group_by(Clase,a18,P220,jp) |> summarise(pj = sum(FEX_C) )

df <- dt_pl[dt_pl$a18 == 1,] |> make_long(Clase,P220,jp)
dagg <- df|>dplyr::group_by(node)|>tally()

df <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)

pl <- ggplot(df, aes(x = x
                      , next_x = next_x
                      , node = node
                      , next_node = next_node
                      , fill = factor(node)
                      , label = paste0(node)))

pl <- pl +geom_sankey(flow.alpha = 0.5,  color = "gray40", show.legend = TRUE)
pl <- pl +geom_sankey_label(size = 3, color = "white", fill= "gray40", hjust = -0.2)

pl <- pl +  theme_bw()
pl <- pl + theme(legend.position = "none")
pl <- pl +  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank())

pl <- pl + scale_fill_viridis_d(option = "inferno")
pl <- pl + labs(title = "Sankey diagram - Justiciable problem declaration")
pl <- pl + labs(caption = "ECSC 2022")
pl <- pl + labs(fill = 'Nodes')

pl

getwd()
openxlsx::write.xlsx(dt, '01.1. general declaration by sex class and age margin.xlsx' )
rm(df,dagg,pl,dt)

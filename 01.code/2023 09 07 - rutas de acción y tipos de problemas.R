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

# Problem type identifiers -------------------------------------------------------------------------

# Justiciable problem --- --- --- 
dt$jp <- 1

# General legal needs --- --- --- 
dt$njg <- 1

dt$njg[dt$P1672 == 'Intentó llegar a un acuerdo directamente con quien tuvo el problema' ] <- 0
dt$njg[dt$P1672 == 'Actuó de forma violenta' ] <- 0

table(dt$njg)

# Strcit legal needs --- --- --- 
dt$nje <- 1

table(dt$P1679)
r_P1679 <- c(
'Prefiere arreglar pacíficamente, a través de diálogo o por sí mismo los problemas',
'Es menos costoso o más ágil que otras soluciones',
'El acuerdo dura más y los resultados son más beneficiosos',
'Hace parte de sus costumbres, usos o tradiciones ',
'Se lo sugirieron',
'Porque el problema no fue tan grave')

table(dt$P1681)
r_P1681 <- c(
'Otro',
'No  había otra opción, estaba en estado de necesidad (hambre de un menor, salud de una persona).',
'Es la forma como se resuelven los problemas aquí.',
'Tenía mucha rabia, se dejó llevar, el otro se lo merecía.',
'Porque el problema no fue tan grave')

table(dt$P1682)
r_P1682 <- c(
'Tenía mucha rabia, se dejó llevar.',
'Se lo sugirieron.',
'Tenía mucha rabia, se dejó llevar.',
'Es la forma como se resuelven los problemas aquí.',
'Otro')

table(dt$P1683)
r_P1683 <- c('No era importante  el problema/ No valía la pena.',
'Razones de fe o filosofía (la justicia divina actuará / Dios ajustará las cargas/ Karma).',
'La persona era conocida o familiar')

vars <- c(
'P1679',
'P1681',
'P1682',
'P1683')

listv <- list(r_P1679,r_P1681,r_P1682,r_P1683)

for(i in 1:length(vars)){
 
  v <- listv[[i]]
  
    for(j in 1:length(v)){

      dt$nje[get(vars[[i]],dt) == v[[j]]] <- 0
  
    }
}

c1 <- dt[dt$caract == 1,] |> group_by(jp,njg,nje,DEPMUNI) |> summarise(fex = sum(FEX_C))
openxlsx::write.xlsx(c1,'02.01 tipos de problemas.xlsx')
# 01.Sand keyplot on total problem, characterization, legal needs and other ------------------------

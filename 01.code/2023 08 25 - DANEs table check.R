# ------------------------------------------------------------------------------------------------ #
# OUT TABLES REPORTING - ECSC 2022
#  2023/08/25
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
  "https://drive.google.com/file/d/197V-R5Dkma-HDQzoCrOSXmY4aMYXrLjO/view" #Individuals problems 
)

files_f <- function(x){
  usethis::create_download_url(x) |>
    url() |>
    readRDS() |>
    tibble::as_tibble()
}

dt_list <- lapply(gd,files_f);rm(gd)

names(dt_list) <- c('dt_p','dt_pj_rt_p','dt_allvisited_inst','dt_pj_rt_pl','dt_pl')

dt_pl <- as.data.frame(dt_list[['dt_pl']])
dt_pj_rt_pl <- dt_list[['dt_pj_rt_pl']]

dt_pj_rt_pl|> group_by(cc) |> summarise(fex = sum(FEX_Cx))
dt_pj_rt_pl|> group_by(caract) |> summarise(fex = sum(FEX_Cx))
dt_pj_rt_pl|> group_by(full_prob) |> summarise(fex = sum(FEX_Cx))

# Tables to replicate - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Cuadro 1. Población de 18 años y más  que han experimentado un problema, desacuerdo, conflicto o disputa durante 2020 y 2021, por tipo de problema.

# National groupings
#
dt <- dt_pj_rt_pl[!duplicated(dt_pj_rt_pl$keyp),]
c11 <- dt |> group_by(DEPMUNI) |> summarise(fex = sum(FEX_Cx, na.rm = TRUE))
c12 <- dt |> group_by(Clase) |> summarise(fex = sum(FEX_Cx, na.rm = TRUE))
c13 <- dt |> group_by(CIUDADES28) |> summarise(fex = sum(FEX_Cx, na.rm = TRUE))

# Problem groupings
#

c1 <- dt_pj_rt_pl[!duplicated(dt_pj_rt_pl$DEPMUNI),'DEPMUNI']

table(dt_pj_rt_pl$cat_labs)
cat_labs <- unique(dt_pj_rt_pl$cat_labs)

dt_list <- list()
for (i in 1:length(cat_labs)){

dt <- dt_pj_rt_pl[dt_pj_rt_pl$cat_labs == cat_labs[i],]
dt <- dt[!duplicated(dt$keyp),]
dt <- dt[!duplicated(dt$keyp),]

c2 <- dt |> group_by(DEPMUNI) |> summarise(fex = sum(FEX_Cx, na.rm = TRUE))
c3 <- merge(c1,c2,all.x = TRUE, by = 'DEPMUNI')

c3$fex <- round(c3$fex/1000,1)
names(c3) <- c('depmu', cat_labs[i] )
dt_list[[i]] <- c3

}

c <- do.call(cbind, dt_list)

# Cuadro 1a. Población de 18 años y más que han experimentado un problema, desacuerdo, conflicto o disputa durante 2020 y 2021, por sexo.

dt |> group_by(DEPMUNI, P220) |> summarise(fex = sum(FEX_Cx, na.rm = TRUE))

# Cuadro 2. Problemas reportados y caracterizados por población de 18 años y más, según ciclo de la encuesta.

dt_pj_rt_pl|> group_by(full_prob) |> summarise(fex = sum(FEX_Cx))
dt_pj_rt_pl[dt_pj_rt_pl$full_prob == 1,]|> group_by(DEPMUNI) |> summarise(fex = (sum(FEX_Cx)))

dt_pj_rt_pl|> group_by(caract) |> summarise(fex = sum(FEX_Cx))
dt_pj_rt_pl[dt_pj_rt_pl$caract == 1,]|> group_by(DEPMUNI) |> summarise(fex = (sum(FEX_Cx)))


dt_pj_rt_pl|> group_by(caract,cc) |> summarise(fex = sum(FEX_Cx))
dt_pj_rt_pl[dt_pj_rt_pl$caract == 1,]|> group_by(DEPMUNI,cc) |> summarise(fex = (sum(FEX_Cx)))

# Cuadro 5a. Total de problemas reportados por nivel de afectación, tipo y desagregación geográfica.

dt_pj_rt_pl[dt_pj_rt_pl$full_prob == 1,] |> group_by(impact) |> summarise(fex = sum(FEX_Cx))
dt_pj_rt_pl[dt_pj_rt_pl$Clase == 'Cabecera' & dt_pj_rt_pl$full_prob == 1,] |> group_by(Clase,impact) |> summarise(fex = (sum(FEX_Cx)))
dt_pj_rt_pl[dt_pj_rt_pl$Clase == 'Centro poblado y rur' & dt_pj_rt_pl$full_prob == 1,] |> group_by(Clase,impact) |> summarise(fex = (sum(FEX_Cx)))

# Cuadro 7. Problemas caracterizados en el ciclo largo que tomaron la ruta institucional, según Instituciones que visitaron para resolver el problema, por tipo de problema. 

inst <- openxlsx::read.xlsx('L:/01.ecsc2022/02.enj2022/2023 06 30 - all_inst.xlsx')
inst <- inst[!duplicated(inst$keypp),]
table(inst$keypp %in% dt_pj_rt_pl$keypp)

dt_pj_rt_pl <- merge(dt_pj_rt_pl,inst[c('institution','keypp')], all.x = TRUE, by = 'keypp')
dt_pj_rt_pl$P1674_1 <- as.character(dt_pj_rt_pl$P1674)
dt_pj_rt_pl$P1674_1[is.na(dt_pj_rt_pl$P1674_1 ) == TRUE] <- dt_pj_rt_pl$institution[is.na(dt_pj_rt_pl$P1674_1 ) == TRUE]

dt <- dt_pj_rt_pl[ dt_pj_rt_pl$cc == 0 ,]
c11 <- dt|> group_by(P1674_1) |> summarise(fex = sum(FEX_Cx))

c11$fex <- round(c11$fex/1000,1)

# Cuadro 6a. Rutas que tomaron las personas frente al problema, desacuerdo, conflicto o disputa, por dominio geográfico.

dt <- dt_pj_rt_pl[ dt_pj_rt_pl$cc == 0 ,]
dt |> group_by(P1672) |> summarise(fex = round(sum(FEX_Cy),1))

# Cuadro 18a. Problemas caracterizados por el ciclo largo, según si se solucionaron o no, por desagregación geográfica.

 dt |> group_by(P1685) |> summarise(fex = round(sum(FEX_Cy),1))




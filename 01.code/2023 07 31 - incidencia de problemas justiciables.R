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

# loads each crime sub chapter to build the incident reporting variable
#
pat <- "https://docs.google.com/uc?id=%s&export=download"
files <- openxlsx::read.xlsx(sprintf(pat, "1P4x_BW5PZz8HJP5KfznhVV7vWZ_FEqKd"))
cap_lab <- as.vector(files$cap)

# loading function
#
files_f <- function(x){haven::read_dta(sprintf(pat,x)) }
cap_list <- lapply(files$id_cap,files_f)
names(cap_list) <- files$cap

crimenames <-  c('ext.dta',
'hurto_ganado.dta',
'hurto_personas.dta',
'hurto_residencia.dta',
'hurto_vehiculos.dta',
'rinas_peleas.dta')

dt_pl$keyc <- paste0(dt_pl$DIRECTORIO,
                     dt_pl$SECUENCIA_ENCUESTA,
                     dt_pl$SECUENCIA_P,
                     dt_pl$ORDEN)

for (i in 1:length(crimenames)){
  crime <- crimenames[i]
  cap_list[[crime]]$keyc <- paste0(
    cap_list[[crime]]$DIRECTORIO,
    cap_list[[crime]]$SECUENCIA_ENCUESTA,
    cap_list[[crime]]$SECUENCIA_P,
    cap_list[[crime]]$ORDEN
  )
  print(table(duplicated(cap_list[[crime]]$keyc)))
  print(table(cap_list[[crime]]$keyc %in% dt_pl$keyc ))
}
rm(crimenames,crime)

dt_pl <- merge(dt_pl, cap_list[['ext.dta']][c('keyc','P1187','P1106')], all.x = TRUE)
dt_pl <- merge(dt_pl, cap_list[['hurto_ganado.dta']][c('keyc','P2073','P2076')], all.x = TRUE)
dt_pl <- merge(dt_pl, cap_list[['hurto_personas.dta']][c('keyc','P1324','P1113')], all.x = TRUE)
dt_pl <- merge(dt_pl, cap_list[['hurto_residencia.dta']][c('keyc','P1228','P1214')], all.x = TRUE)
dt_pl <- merge(dt_pl, cap_list[['hurto_vehiculos.dta']][c('keyc','P1110','P1238')], all.x = TRUE)
dt_pl <- merge(dt_pl, cap_list[['rinas_peleas.dta']][c('keyc','P1294','P1227')], all.x = TRUE)

rm(i,pat,cap_lab,cap_list,files_f)
dt_plx  <- dt_pl
dt_pl <- dt_plx

## Justiciable problem count and id ----------------------------------------------------------------
## 
sum(dt_pl$FEX_C)
table(dt_pl$count); table(is.na(dt_pl$count))  #Checks total against total declaration table

# dt_pl$count[is.na(dt_pl$count) == TRUE] <- 0
# 
table(dt_pl$count)

dt_pl$jp <- dt_pl$count/dt_pl$count
table(dt_pl$jp[dt_pl$P5785 > 17]); table(!duplicated(dt_pj_rt_pl$keyp))
dt_pl$jp[is.na(dt_pl$jp) == TRUE] <- 0

## 18 and above indicator --------------------------------------------------------------------------
## 
dt_pl$a18 <- 0
dt_pl$a18[dt_pl$P5785 > 17] <- 1
table(dt_pl$a18, dt_pl$jp)

dt_pl <- dt_pl[dt_pl$P5785 > 14,]

# 00.02 DEFINES RELEVANT GROUPING VARIABLES --------------------------------------------------------
#

frec <- function(x){
  print('frequencies')
  print(table(dt_pl[,x]))
  print('numeric frequencies')
  print(table(as.numeric(dt_pl[,x])))
  print('NAs frequencies')
  print(table(is.na(dt_pl[,x])))

}

# Demographics - - - -
# 
# P3038 romantic attraction
# 
frec('P3038')
dt_pl$P3038 <- as.numeric(dt_pl$P3038)
dt_pl$hetero <- NA
dt_pl$hetero[dt_pl$P3038 < 3 ] <- 1
dt_pl$hetero[dt_pl$P3038 > 2 ] <- 0
table(dt_pl$hetero)

# P220 sex
# 
frec('P220')
dt_pl$P220 <- as.numeric(dt_pl$P220)
dt_pl$P220[dt_pl$P220 == 2 ] <- 0
table(dt_pl$P220)

# P3039 gender
# 
frec('P3039')
dt_pl$P3039 <- as.numeric(dt_pl$P3039)
dt_pl$cis <- NA
dt_pl$cis[dt_pl$P3039 < 3 ] <- 1
dt_pl$cis[dt_pl$P3039 > 2 ] <- 0
frec('cis')

# P5785 age
# 
frec('P5785')
dt_pl$old <- 0
dt_pl$old[dt_pl$P5785 > 59] <- 1
frec('old')

# Clase household type
# 
frec('Clase')
table(is.na(dt_pl$Clase))
dt_pl$Clase[dt_pl$Clase == 'Cabecera'] <- 1
dt_pl$Clase[dt_pl$Clase == 'Centro poblado y rur'] <- 0

# P1988 electricity in the household
# 
frec('P1988')
dt_pl$P1988 <- as.numeric(dt_pl$P1988)
dt_pl$P1988[dt_pl$P1988 == 2] <- 0

# P1988S1 household strata
# 
frec('P1988S1')
dt_pl$strata <- NA
dt_pl$strata[dt_pl$P1988S1 > 3 ] <- 1
dt_pl$strata[dt_pl$P1988S1 < 4 ] <- 0

frec('strata')
table(dt_pl$strata, dt_pl$P1988S1)

# P1989 household ownership
# 
frec('P1989')
dt_pl$ownedh <- 0
dt_pl$ownedh[dt_pl$P1989 == 'Propia' ] <- 1
table( dt_pl$P1989,dt_pl$ownedh)

# P3303 internet access/connection in the last year
# 
frec('P3303')
dt_pl$P3303 <- as.numeric(dt_pl$P3303)
dt_pl$P3303[dt_pl$P3303 == 2 ] <- 0

# P6080 self recognition
# 
frec('P6080')
dt_pl$recon <- 0
dt_pl$recon[dt_pl$P6080 != 'Ninguno de los anteriores' ] <- 1
table(dt_pl$recon, dt_pl$P6080)

# P6210 highest educational level achieved
# 
frec('P6210')
table(dt_pl$P5785[is.na(dt_pl$P6210) == TRUE])
dt_pl$edug <- 0 # all individuals under 14 have NAs in this feature
dt_pl$edug[dt_pl$P5785 < 15] <- NA
dt_pl$edug[dt_pl$P6210 == 'Superior o Universitaria' ] <- 1
dt_pl$edug[dt_pl$P6210 == 'Media (10-13)' ] <- 1
dt_pl$edug <- factor(dt_pl$edug)

table(dt_pl$edug)
frec('edug')

# P6210S1 education degree
# 
# P1366 Marital status
# 
frec('P1366')
table(dt_pl$P5785[is.na(dt_pl$P1366) == TRUE])
dt_pl$single <- 0 # all individuals under 14 have NAs in this feature
dt_pl$single[dt_pl$P5785 < 15] <- NA
dt_pl$single[dt_pl$P1366 == 'Está separado(a) o divorciado(a)' ] <- 1
dt_pl$single[dt_pl$P1366 == 'Está soltero(a)' ] <- 1
dt_pl$single[dt_pl$P1366 == 'Está viudo(a)' ] <- 1
table(dt_pl$P1366,dt_pl$single)

# P756 Birth place
# 
# 
frec('P756')
table(dt_pl$P5785[is.na(dt_pl$P756) == TRUE])
dt_pl$born_col <- 0 # all individuals under 14 have NAs in this feature
dt_pl$dt_pl[dt_pl$P5785 < 15] <- NA
dt_pl$born_col[dt_pl$P756 != 'En otro país' ] <- 1
table(dt_pl$P756,dt_pl$born_col)

# Disabilities - - - - 
# P1906S1 hearing
# P1906S2 speaking
# P1906S3 seeing
# P1906S4 moving
# P1906S5 handling objects
# P1906S6 memory and decisions
# P1906S7 self maintenance
# P1906S8 social interactions
# P1906S9 respiratory and heart limitations
# 
dis <- c(
 'P1906S1', 
 'P1906S2', 
 'P1906S3',
 'P1906S4',
 'P1906S5',
 'P1906S6',
 'P1906S7',
 'P1906S8',
 'P1906S9')

for (i in 1:length(dis)){
  print(frec(dis[i]))
}


for (i in 1:length(dis)){
  disi <- dis[i]
  dt_pl[,paste0(disi,'_',i)] <- as.numeric(dt_pl[,disi])
  print(frec(paste0(disi,'_',i)))
  dt_pl[,paste0(disi,'_',i)][dt_pl[,paste0(disi,'_',i)] < 4] <- 1
  dt_pl[,paste0(disi,'_',i)][dt_pl[,paste0(disi,'_',i)] == 4] <- 0
  
  print(table(dt_pl[,disi],dt_pl[,paste0(disi,'_',i)]))
  
}

dt_pl$dis <- rowSums(dt_pl[paste0(dis,'_',seq(1:length(dis)))], na.rm = TRUE)
dt_pl$dis  <- dt_pl$dis / dt_pl$dis 
dt_pl$dis[is.na(dt_pl$dis) == TRUE] <- 0
dt_pl$dis[dt_pl$P5785 < 15] <- NA
frec('dis')

# Socio economic - - - -
# 
# P1365 activities (work)
# 
frec('P1365')
dt_pl$pea <- 0
dt_pl$pea[dt_pl$P1365 == 'Trabajando' ] <- 1
dt_pl$pea[dt_pl$P1365 == 'Buscando trabajo' ] <- 1

table(dt_pl$P1365,dt_pl$pea)
dt_pl$pea[dt_pl$P5785 < 15] <- NA

frec('pea')

# P3105 security perception (local)
# 
frec('P3105')
dt_pl$safe_local <- 0
dt_pl$safe_local[dt_pl$P3105 == 'Muy Seguro(a)' ] <- 1
dt_pl$safe_local[dt_pl$P3105 == 'Seguro(a)' ] <- 1

table(dt_pl$P3105,dt_pl$safe_local)
dt_pl$safe_local[dt_pl$P5785 < 15] <- NA
frec('safe_local')

# P3106 security perception when walking alone at night
# 
frec('P3106')
dt_pl$safe_WaN <- 0
dt_pl$safe_WaN[dt_pl$P3106 == ' Muy seguro(a)' ] <- 1
dt_pl$safe_WaN[dt_pl$P3106 == 'Seguro(a)' ] <- 1

table(dt_pl$P3106,dt_pl$safe_WaN)
dt_pl$safe_WaN[dt_pl$P5785 < 15] <- NA
frec('safe_WaN')

# P3107 security perception (municipality/city)
# 
frec('P3107')
dt_pl$safe_city <- 0
dt_pl$safe_city[dt_pl$P3107 == 'Muy Seguro(a)' ] <- 1
dt_pl$safe_city[dt_pl$P3107 == 'Seguro(a)' ] <- 1

table(dt_pl$P3107,dt_pl$safe_city)
dt_pl$safe_city[dt_pl$P5785 < 15] <- NA
frec('safe_city')

# P564 prospects on being a victim in the future
#
frec('P564')
dt_pl$P564 <- as.numeric(dt_pl$P564)
dt_pl$P564[dt_pl$P564 == 2 ] <- 0
dt_pl$P564[dt_pl$P5785 < 15] <- NA

# Subjective Well-being - - - -
#
# P3503S1 SWB life
# P3503S2 SWB health
# P3503S3 SWB economic outlook
# P3503S4 SWB work
# P3503S5 SWB emotional outlook
# P3503S6 SWB relationships

swvars <- c('P3503S1','P3503S2','P3503S3','P3503S4','P3503S5','P3503S6')

lapply(swvars, function(x){
  print(table(dt_pl[,x]))
  print(table(is.na(dt_pl[,x])))})

for (i in 1:length(swvars)){
  swbi <- swvars[i]
  dt_pl[,paste0(swbi,'_',i)] <- as.numeric(dt_pl[,swbi])
  dt_pl[,paste0(swbi,'_',i)][dt_pl[,paste0(swbi,'_',i)] == 9] <- NA
  print(frec(paste0(swbi,'_',i)))
  dt_pl[,paste0(swbi,'_',i)][dt_pl[,paste0(swbi,'_',i)] < 4] <- 1
  dt_pl[,paste0(swbi,'_',i)][dt_pl[,paste0(swbi,'_',i)] > 3] <- 0
  
  print(table(dt_pl[,swbi],dt_pl[,paste0(swbi,'_',i)]))
  
}

dt_pl$swbi <- rowSums(dt_pl[paste0(swvars,'_',seq(1:length(swvars)))], na.rm = FALSE)
dt_pl$swbi[dt_pl$swbi > 0] <- 1
table(dt_pl$swbi)

for (i in 1:length(swvars)){
  # print(table(dt_pl[,swvars[i]],dt_pl$swbi))
   print(table(dt_pl[,swvars[i]]))
  # print(table(dt_pl[,swvars[i]],is.na(dt_pl$swbi)))
}

table(is.na(dt_pl$swbi))
table(dt_pl$P5785[is.na(dt_pl$swbi) == TRUE])

# Street abuse and sexual abuse experiences - - - -
# 
# P3302S1 street harassment of sexual nature
# P3302S2 body shame, sexual preferences shame
# P3302S3 sexual intentions
# P3302S4 non-consented kissing/hugging
# P3302S5 sexual proposal/ force dating
# P3302S6 unwanted sexual gifts
# P3302S7 on-line sexual harassment
# P3302S8 sexual public exhibition
# P1976S13 street sexual harassment 
# P1976S14 sexual violence or aggression
# 

abuse <- c(
  'P3302S1', 
  'P3302S2', 
  'P3302S3',
  'P3302S4',
  'P3302S5',
  'P3302S6',
  'P3302S7',
  'P1976S13',
  'P1976S14')

for (i in 1:length(abuse)){
  abusei <- abuse[i]
  dt_pl[,paste0(abusei,'_',i)] <- as.numeric(dt_pl[,abusei])
  print(frec(paste0(abusei,'_',i)))
  dt_pl[,paste0(abusei,'_',i)][dt_pl[,paste0(abusei,'_',i)] == 3] <- NA
  dt_pl[,paste0(abusei,'_',i)][dt_pl[,paste0(abusei,'_',i)] == 2] <- 0
  
  print(table(dt_pl[,abusei],dt_pl[,paste0(abusei,'_',i)]))
  
}

dt_pl$abuse <- rowSums(dt_pl[paste0(abuse,'_',seq(1:length(abuse)))], na.rm = FALSE)
table(dt_pl$abuse)
dt_pl$abuse[dt_pl$abuse > 0] <- 1

frec('abuse')

for (i in 1:length(abuse)){
  print(table(dt_pl[,abuse[i]],dt_pl$abuse))
  print(table(dt_pl[,abuse[i]]))
}
 
# Crimes and victimization - - - -
#
# P541 household theft 2022
# P523 vehicle theft
# P525 personal object theft
# P526 physical aggression an quarrels 
# P528 extortion
# P1959 animal theft
# P195 other
# P1392 household theft 2021
# P1960 animal theft
# P1179 vehicle theft
# P1343 personal object theft
# P1315 physical aggression an quarrels 
# P1286 extortion
# P1976S1 other

table(dt_pl$vic); table(is.na(dt_pl$vic))
dt_pl$vic[is.na(dt_pl$vic)] <- 0 # As the Vic variable was build under the declaration table
# all individuals who experienced a Vic circumstance  also declared JP. This is a problem.

tonumber <- function(x){as.numeric(dt_pl[,x])}
vicvars <- c('P541', 'P523','P525','P526','P528','P1959','P1956',#2022
             'P1392','P1960','P1179','P1343','P1315','P1286','P1976S1' #2021
)


num <- lapply(vicvars, tonumber)
names(num) <- vicvars

lapply(vicvars, function(x){table(is.na(dt_pl[,x]))})

for(i in 1:length(vicvars)){
  var <- vicvars[i]
  dt_pl[,var] <- num[[i]]
  print(table(dt_pl[,var]))
  dt_pl[,var][dt_pl[,var] == 2] <- 0
  print(table(dt_pl[,var]))
}

rm(tonumber,num,i,vicvars)
dt_pl$vic_2022 <- rowSums(dt_pl[c("P541",
                                  "P523",
                                  "P525",
                                  "P526",
                                  "P528",
                                  "P1959",
                                  "P1956"
)], na.rm = FALSE)

dt_pl$vic_2021 <- rowSums(dt_pl[c('P1392',
                                  'P1960',
                                  'P1179',
                                  'P1343',
                                  'P1315',
                                  'P1286',
                                  'P1976S1'
)], na.rm = FALSE)

table(dt_pl$vic_2022)
table(dt_pl$vic_2021)

table(is.na(dt_pl$vic_2021))
table(is.na(dt_pl$vic_2022),dt_pl$P5785)

table(dt_pl$vic_2021)

dt_pl$vic_2021[dt_pl$vic_2021 > 0] <- 1
dt_pl$vic_2022[dt_pl$vic_2022 > 0] <- 1

sum(dt_pl$FEX_C[dt_pl$vic_2022 != 0],na.rm = TRUE)
sum(dt_pl$FEX_C[dt_pl$vic_2021 != 0], na.rm = TRUE)

table(dt_pl$vic_2021,dt_pl$jp)
table(dt_pl$vic_2022,dt_pl$jp)

table(dt_pl$P5785[is.na(dt_pl$vic_2022) == TRUE])
table(dt_pl$P5785[is.na(dt_pl$vic_2021) == TRUE])

frec('vic_2022')
table(dt_pl$P5785)
frec('vic_2021')

# Physical aggression - - - -
# 
# P3115S1 push
# P3115S2 hit
# P3115S3 object/weapon
# P3115S4 other

phyvars <- c('P3115S1','P3115S2','P3115S3','P3115S4')

lapply(phyvars, function(x){table(is.na(dt_pl[,x]))})
lapply(phyvars, function(x){table((dt_pl[,x]))})

for(i in 1:length(phyvars)){
  var <- phyvars[i]
  dt_pl[,var] <- as.numeric(dt_pl[,var] )
  print(table(dt_pl[,var]))
  print(table(is.na(dt_pl[,var])))
  dt_pl[,var][dt_pl[,var] == 2] <- 0
  print(table(dt_pl[,var]))
}

dt_pl$physicalviolence <- rowSums(dt_pl[,phyvars], na.rm = FALSE)
dt_pl$physicalviolence[dt_pl$physicalviolence > 0 ] <- 1
table(dt_pl$physicalviolence)

rm(phyvars,var,i)
dt_pl$physicalviolence[dt_pl$P5785 < 15] <- NA
frec('physicalviolence')

# Crime reporting - - - -
#
# P1228 household theft
# P1214 household theft reasons to avoid reporting
# P2073 animal theft
# P2076 animal theft reasons to avoid reporting
# P1238 vehicle theft
# P1110 vehicle theft reasons to avoid reporting
# P1324 personal object theft
# P1113 personal object theft reasons to avoid reporting
# P1294 physical aggression an quarrels 
# P1227 physical aggression an quarrels reasons to avoid reporting
# P1187 extortion
# P1106 extortion reasons to avoid reporting

reportvars <- c('P1228','P2073','P1238','P1324','P1294','P1187')
crimevars  <- c('P1392','P1960','P1179','P1343','P1315','P1286')

for (i in 1:length(reportvars)){print(table(dt_pl[,reportvars[i]],dt_pl[,crimevars[i]]))}

lapply(reportvars, function(x){table(is.na(dt_pl[,x]))})
lapply(reportvars, function(x){table((dt_pl[,x]))})

for(i in 1:length(reportvars)){
  var <- reportvars[i]
  dt_pl[,var] <- as.numeric(dt_pl[,var] )
  dt_pl[,var][dt_pl[,var] == 2] <- 0
  print(table(dt_pl[,var]))
  print(class(dt_pl[,var]))

}
dt_pl$crimereporting <- rowSums(dt_pl[reportvars],na.rm = TRUE)
table((dt_pl$crimereporting))

dt_pl$crimereporting[dt_pl$vic == 0 ] <- NA
dt_pl$crimereporting[dt_pl$P5785 < 15 ] <- NA
dt_pl$crimereporting[dt_pl$crimereporting > 0 ] <- 1

table(dt_pl$crimereporting,dt_pl$vic)

dt_pl$physicalviolence[dt_pl$physicalviolence > 0 ] <- 1
table(dt_pl$physicalviolence)

rm(reportvars,var,i,crimevars)

# Contribution to security - - - - 
# 
# P1182S1 Police
# P1182S2 Military
# P1182S3 Major
# P1181S1 Prosecutor office
# P1181S2 Judges
# P3317S1 ICBF
# P3317S2 Police inspections
# P3317S3 Conciliation centres

conttibutionvars  <- c('P1182S1','P1182S2','P1182S3','P1181S1','P1181S2','P3317S1','P3317S2','P3317S3')

lapply(conttibutionvars, function(x){table(is.na(dt_pl[,x]))})
lapply(conttibutionvars, function(x){table((dt_pl[,x]))})

for(i in 1:length(conttibutionvars)){
  var <- conttibutionvars[i]
  dt_pl[,var] <- as.numeric(dt_pl[,var] )
  dt_pl[,var][dt_pl[,var] == 2] <- 0
  dt_pl[,var][dt_pl[,var] == 3] <- 0
  print(table(dt_pl[,var]))

}

# Mean differences --------------------------------------------------------------------------------

## Demographics
#
svars <- c(
'P220' ,# sex
'hetero', # romantic attraction
'cis' ,# gender
'old', # age
'single', # Marital status
'edug' ,# highest educational level achieved
'recon', # ethnic recognition
'dis', # at least one kind of disabilities
'born_col', # born in Colombia
'Clase' ,# rural urban class
'P1988' ,# electricity in the household
'strata' , # utilities strata
'ownedh', # household ownership
'P3303') # internet connection

table(dt_pl$P6080)

lapply(svars, function(x) {table(dt_pl[dt_pl$a18 == 1,x])})

dt <- dt_pl[dt_pl$a18 == 1, ]

stats1 <- lapply(svars, function(x) {
             wilcox.test(
                     dt[ is.na(dt[x]) == FALSE,'jp'] ~ 
                     dt[ is.na(dt[x]) == FALSE,x])
  })

stats2 <- list()

for (i in 1:length(svars)) {
x <-  svars[i]

stats2[[i]] <- as.data.frame(
  dt[ is.na(dt[x]) == FALSE,] |>
  group_by(across(all_of(x))) |>
  get_summary_stats(jp,show = c("mean", "sd", "se")))

}

names(stats1) <- svars
names(stats2) <- svars

stats_dt1 <- data.frame(
  statistic = NA,
  p.value = NA, 
  null.value = NA, 
  alternative = NA,
  variable = NA
  
)

for (i in 1:length(svars)) {
  aux_dt <-  data.frame(
    statistic = stats1[[i]]$statistic,
    p.value = round(stats1[[i]]$p.value,4), 
    null.value = as.character(stats1[[i]]$null.value), 
    alternative = stats1[[i]]$alternative
  )
  
  aux_dt$variable <- svars[i]
  stats_dt1 <- rbind(stats_dt1,aux_dt)
  rm(aux_dt)
}

stats_dt2 <- data.frame(
                       cat = NA,
                       variable = NA,
                       n = NA, 
                       mean = NA, 
                       sd = NA,
                       se = NA
                       )

for (i in 1:length(svars)) {
  aux_dt <- as.data.frame(stats2[[i]])
  names(aux_dt) <- names(stats_dt2)
  aux_dt$variable <- svars[i]
  stats_dt2 <- rbind(stats_dt2,aux_dt)
  
  rm(aux_dt)

}

stats_dt1 <- stats_dt1[is.na(stats_dt1$statistic) == FALSE,]
stats_dt2 <- stats_dt2[is.na(stats_dt2$cat) == FALSE,]
stats_dt2 <- merge(stats_dt1,stats_dt2, all.y = TRUE)

openxlsx::write.xlsx(stats_dt2, 'stats_dt2.xlsx')
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

# 02. CLASS, AGE MARGIN, SEX,EDUCATION, DECLARATION ------------------------------------------------
# 
dt <- dt_pl |> group_by(Clase,a18,P220,P6210,jp) |> summarise(pj = sum(FEX_C) )
getwd()
openxlsx::write.xlsx(dt, '01.2. general declaration by sex class and age margin edu.xlsx' )
rm(df,dagg,pl,dt)

table(dt_pl$P6210)



table(dt_pl$vic_2021,dt_pl$jp)
table(dt_pl$vic_2022,dt_pl$jp)

dt <- dt_pl |> group_by(Clase,a18,P220,edug,P5785,P1988S1,vic) |> summarise(pj = mean(jp) )

table(dt$P1988S1)
dt$strata <- 0

dt$strata[dt$P1988S1 > 5 ] <- 1
dt$strata <- factor(dt$strata)

mean(dt$pj[dt$P220 == 'Mujer'& dt$a18 == 1 & is.na(dt$P1988S1) == FALSE ])
mean(dt$pj[dt$P220 == 'Hombre'& dt$a18 == 1 & is.na(dt$P1988S1) == FALSE ])



# Class
wilcox.test(pj ~ Clase, data = dt[dt$a18 == 1,], exact = FALSE)
wilcox.test(jp ~ Clase, data = dt_pl[dt_pl$a18 == 1,], exact = FALSE)

dt_pl[dt_pl$a18 == 1,] |> group_by(Clase) |> summarise(pj = mean(jp) )

# Sex
wilcox.test(pj ~ P220, data = dt[dt$a18 == 1,], exact = FALSE)
wilcox.test(jp ~ P220, data = dt_pl[dt_pl$a18 == 1,], exact = FALSE)

dt_pl[dt_pl$a18 == 1,] |> group_by(P220) |> summarise(pj = mean(jp) )

# Education
wilcox.test(pj ~ edug, data = dt[dt$a18 == 1,], exact = FALSE)
wilcox.test(jp ~ edug, data = dt_pl[dt_pl$a18 == 1,], exact = FALSE)

dt_pl[dt_pl$a18 == 1,] |> group_by(edug) |> summarise(pj = mean(jp) )

dt_pl[dt_pl$a18 == 1,] |> group_by(vic) |> summarise(pj = mean(jp) )

library("ggpubr")
ggboxplot(dt[dt$a18 == 1,], x = "edug", y = "pj", 
          color = "edug", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")



dt$a <- (dt$pj - mean(dt$pj)) / sd(dt$pj)
hist(dt$a)
shapiro.test(dt$a[1:5000])
shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(runif(100, min = 2, max = 4))



with(dt[dt$a18 == 1,], shapiro.test(pj[Clase == "Centro poblado y rur"])) # p = 0.6

var.test(pj ~ Clase, data = dt[dt$a18 == 1,])

t.test(pj ~ Clase, data = dt[dt$a18 == 1,], var.equal = TRUE)


dt <- dt_pl |> group_by(Clase,a18,P220,P6210,P5785,P1988S1) |> summarise(pj = mean(jp) )

output <- dt_pl |>
  select(-keyp) |>
  group_by(Clase, P220)|>
  summarize_at(
    vars(-group_cols(), -Session),
    list(t.test = ~ list(t.test(. ~ Session))))

output





## Subjective well-being ----------------------------------------------------------------------------
## 

swvars <- c('P3503S1','P3503S2','P3503S3','P3503S4','P3503S5','P3503S6')
swvarsl <- c('la vida en general','su estado de salud','su situación económica',
             'su situación laboral','su vida emocional','sus relaciones interpersonales')

lapply(swvars, function(x){table(dt_pl[,x])})

for(i in 1:length(swvars)){
  print(table(dt_pl[,var]))
  var <- swvars[i]
  dt_pl[,var][is.na(dt_pl[,var]) == TRUE] <- 0
  print(table(dt_pl[,var]))
  print(table(dt_pl$P5785,dt_pl[,var]))
}


for (i in 1:length(swvars)) {
  
  x <- swvars[i]
  
  p <- ggplot(dt_pl[dt_pl[,x] != 0 & dt_pl[,x] != 9,], 
              aes(x = dt_pl[,x][dt_pl[,x] != 0 & dt_pl[,x] != 9], 
                  after_stat(density), fill = as.factor(jp))) +
    geom_histogram( bins = 5,alpha = 0.7, position = "identity") + 
    scale_color_manual(values=c( "#E7B800", "#00AFBB"))+
    scale_fill_manual('',
                      values=c( "#E7B800", "#00AFBB"),
                      labels=c("No declarante","Declarante"))+
    theme_minimal() + 
    theme(legend.position="top") +
    labs(x= paste0('Valoracion subjetiva de ',swvarsl[i]), y = "Densidad")
  
  print(p)
  rm(p)
}

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
library(Hmisc) # sets variable labels

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
  "https://drive.google.com/file/d/1JxTGqtd-oSfQIkbdfoQrzmMn0aMcewAj/view", #Problems
  "https://drive.google.com/file/d/1E6BV8_8cZm1nNUQbxg6IvGulYBbqcB3u/view", #Institutions
  "https://drive.google.com/file/d/17ghCbHsjgl5FHD75rf9U8wecAnnxpsx6/view"  #Individuals problems 
    )

files_f <- function(x){
    usethis::create_download_url(x) |>
    url() |>
    readRDS() |>
    tibble::as_tibble()
  }

dt_list <- lapply(gd,files_f);rm(gd)
names(dt_list) <- c('dt_p','dt_pj_rt_p','dt_allvisited_inst','dt_p_pj')

dt_p <- dt_list[['dt_p']]
dt_pj_rt_p <- dt_list[['dt_pj_rt_p']]
dt_allvisited_inst <- dt_list[['dt_allvisited_inst']]
dt_p_pj <- dt_list[['dt_p_pj']]

names(dt_pj_rt_p)[names(dt_pj_rt_p) == 'FEX_C.x'] <- 'FEX_Cx'
names(dt_pj_rt_p)[names(dt_pj_rt_p) == 'FEX_C.y'] <- 'FEX_Cy'

dt_pj_rt_p <- as.data.frame( dt_pj_rt_p[ , order(names(dt_pj_rt_p))] )
dt_p_pj <- as.data.frame( dt_p_pj[ , order(names(dt_p_pj))] )

# There are two (2) types of expansion factors. One comming from the individuals frame (fex_C.y)
# and one comming from the problem declaration table (fex_c.x) given the non existance of some
# records in the declaration frame, the fexc from the individuals table tends to group more 
# records.
# 
View((dt_pj_rt_p[c('FEX_Cx','FEX_Cy')])) 

# Problem-individual records outside the declarion framework dont have route data. Thus, in the
# clasiffication problem they were leftout of the full problem dummy. This issue is fixed by 
# imputing the resulting NAs
# 

table(is.na(dt_pj_rt_p$full_prob))
dt_pj_rt_p$full_prob[is.na(dt_pj_rt_p$full_prob) == TRUE ] <- 0
table((dt_pj_rt_p$full_prob))

sum(dt_pj_rt_p$FEX_Cx[dt_pj_rt_p$full_prob == 1])
sum(dt_pj_rt_p$FEX_Cy[dt_pj_rt_p$full_prob == 1])

# When excluding those records, the total expansion factor is the same for the individual and the 
# problem table.

## 01.00 variable categories -----------------------------------------------------------------------

pat <- "https://docs.google.com/uc?id=%s&export=download"
factor_labs <- openxlsx::read.xlsx(sprintf(pat, "14_Cv3lBwor0n6snth60zPlGsOOoBo1qL"))

var <- unique(factor_labs$var)

head(factor_labs)

for(i in 1:length(var)){
levs <- factor_labs$a[factor_labs$var == var[i]]
labs <- factor_labs$b[factor_labs$var == var[i]]

dt_pj_rt_p[,var[i]] <- factor(x = dt_pj_rt_p[,var[i]], levels = levs, lab = labs)
str(dt_pj_rt_p[,var[i]])
}

var1 <- var[var %in% names(dt_p_pj)]

for(i in 1:length(var1)){
  levs <- factor_labs$a[factor_labs$var == var1[i]]
  labs <- factor_labs$b[factor_labs$var == var1[i]]
  
  dt_p_pj[,var1[i]] <- factor(x = dt_p_pj[,var1[i]], levels = levs, lab = labs)
  str(dt_p_pj[,var1[i]])
}

## 02.00 variable lables ---------------------------------------------------------------------------

pat <- "https://docs.google.com/uc?id=%s&export=download"
labs <- openxlsx::read.xlsx(sprintf(pat, "1Wvz50SQuKe7MSKTBWScyi4_OkmbXJ-rY"))
var.labels <- data.frame(names = names(dt_p_pj))
table(var.labels$names %in% labs$names)

var.labels <- merge(var.labels, labs, all.x =  TRUE)

dt_p_pj <- labelled::set_variable_labels(dt_p_pj, .labels = substr(var.labels$lab, 1, 75))
names(dt_p_pj)
table(dt_p_pj$count)

dt_pj_rt_p <- dt_pj_rt_p[c(
  "keyh",
  "keyp",
  "keypp",
  "NRO_ENCUESTA",
  "ORDEN",
  "SECUENCIA_ENCUESTA",
  "SECUENCIA_P",
  "DIRECTORIO",
  "FEX_Cx",
  "FEX_Cy",
  "Clase",
  "month",
  "month_q",
  "year_q",
  "year",
  "CIUDADES28",
  "DEPMUNI",
  "full_prob",
  "nj_count1",
  "nj_prio",
  "p_type",
  "cat_en",
  "cat_id",
  "cat_lab",
  "cat_labs",
  "type_en",
  "type_id",
  "type_lab",
  "type_labs",
  "impact",
  "impact_q",
  "P1672",
  "P1674",
  "P1675",
  "P1676",
  "P1676S2",
  "P1676S3",
  "P1677",
  "P1678",
  "P1679",
  "P1680",
  "P1681",
  "P1682",
  "P1683",
  "P1684",
  "P1685",
  "P1686",
  "P1687",
  "P1688",
  "P1689",
  "P1690",
  "P3302S1",
  "P3302S2",
  "P3302S3",
  "P3302S4",
  "P3302S5",
  "P3302S6",
  "P3302S7",
  "P3302S8",
  "P3317S1",
  "P3317S2",
  "P3317S3",
  "P1181S1",
  "P1181S2",
  "P1182S1",
  "P1182S2",
  "P1182S3",
  "P3119",
  "P3502",
  "P3503S1",
  "P3503S2",
  "P3503S3",
  "P3503S4",
  "P3503S5",
  "P3503S6",
  "P1115",
  "P5785",
  "P6210",
  "P6210S1",
  "P3303",
  "P1366",
  "P1363",
  "P1364",
  "P1365",
  "P1987",
  "P1988",
  "P1988S1",
  "P1989",
  "P1990",
  "P1402",
  "P1403",
  "P753",
  "P753S3",
  "P755",
  "P755S3",
  "P756",
  "P756S3",
  "P1662",
  "P2057",
  "P2059",
  "P2061",
  "P220",
  "P3038",
  "P3039",
  "P6080",
  "P1179",
  "P1179S1",
  "P1179S3",
  "P1286",
  "P1315",
  "P1315S1",
  "P1315S3",
  "P1343",
  "P1343S1",
  "P1343S3",
  "P1353S1",
  "P1353S2",
  "P1353S3",
  "P1353S4",
  "P1353S5",
  "P1353S6",
  "P1358S1",
  "P1358S10",
  "P1358S11",
  "P1358S12",
  "P1358S13",
  "P1358S14",
  "P1358S15",
  "P1358S2",
  "P1358S3",
  "P1358S4",
  "P1358S5",
  "P1358S6",
  "P1358S7",
  "P1358S8",
  "P1358S9",
  "P1361S1",
  "P1361S10",
  "P1361S11",
  "P1361S12",
  "P1361S13",
  "P1361S14",
  "P1361S15",
  "P1361S16",
  "P1361S2",
  "P1361S3",
  "P1361S4",
  "P1361S5",
  "P1361S6",
  "P1361S7",
  "P1361S8",
  "P1361S9",
  "P1392",
  "P1392S1",
  "P1392S3",
  "P1611S1",
  "P1612S1",
  "P1772",
  "P1906S1",
  "P1906S2",
  "P1906S3",
  "P1906S4",
  "P1906S5",
  "P1906S6",
  "P1906S7",
  "P1906S8",
  "P1906S9",
  "P1956",
  "P1959",
  "P1960",
  "P1960S1",
  "P1960S2",
  "P1976S1",
  "P1976S13",
  "P1976S14",
  "P1976S2",
  "P1976S4",
  "P1976S5",
  "P1976S6",
  "P1976S7",
  "P1976S8",
  "P1976S9",
  "P3105",
  "P3106",
  "P3107",
  "P3108",
  "P3109S1",
  "P3109S10",
  "P3109S11",
  "P3109S12",
  "P3109S13",
  "P3109S14",
  "P3109S15",
  "P3109S16",
  "P3109S17",
  "P3109S18",
  "P3109S2",
  "P3109S3",
  "P3109S4",
  "P3109S5",
  "P3109S6",
  "P3109S7",
  "P3109S9",
  "P3111S1",
  "P3111S2",
  "P3111S3",
  "P3111S4",
  "P3111S5",
  "P3111S6",
  "P3111S7",
  "P3111S8",
  "P3111S9",
  "P3112S1",
  "P3112S10",
  "P3112S11",
  "P3112S12",
  "P3112S13",
  "P3112S14",
  "P3112S15",
  "P3112S2",
  "P3112S3",
  "P3112S4",
  "P3112S5",
  "P3112S6",
  "P3112S7",
  "P3112S8",
  "P3112S9",
  "P3113S1",
  "P3113S10",
  "P3113S11",
  "P3113S12",
  "P3113S13",
  "P3113S14",
  "P3113S15",
  "P3113S16",
  "P3113S17",
  "P3113S18",
  "P3113S19",
  "P3113S2",
  "P3113S20",
  "P3113S21",
  "P3113S22",
  "P3113S3",
  "P3113S4",
  "P3113S5",
  "P3113S6",
  "P3113S7",
  "P3113S8",
  "P3113S9",
  "P3114",
  "P3115S1",
  "P3115S2",
  "P3115S3",
  "P3115S4",
  "P3121",
  "P3122S1",
  "P3122S2",
  "P3122S3",
  "P3122S4",
  "P3123",
  "P3124",
  "P3125S1",
  "P3125S2",
  "P3125S3",
  "P3125S4",
  "P3125S5",
  "P3125S6",
  "P3126",
  "P3127",
  "P3128S1",
  "P3128S2",
  "P3128S3",
  "P3128S4",
  "P3128S5",
  "P3128S6",
  "P3128S7",
  "P3128S8",
  "P3129",
  "P33010",
  "P33010S1",
  "P33010S2",
  "P33011",
  "P33011S1",
  "P33011S2",
  "P3304",
  "P3305S1",
  "P3305S2",
  "P3305S3",
  "P3305S4",
  "P3305S5",
  "P3306",
  "P3306S1",
  "P3306S2",
  "P3307",
  "P3307S1",
  "P3307S2",
  "P3308",
  "P3308S1",
  "P3308S2",
  "P3309",
  "P3309S1",
  "P3309S2",
  "P3312",
  "P3313",
  "P3314",
  "P3315",
  "P3316S1",
  "P3316S10",
  "P3316S11",
  "P3316S12",
  "P3316S13",
  "P3316S2",
  "P3316S3",
  "P3316S4",
  "P3316S5",
  "P3316S6",
  "P3316S7",
  "P3316S8",
  "P3316S9",
  "P3318",
  "P3319S1",
  "P3319S2",
  "P3320",
  "P3321",
  "P3322",
  "P3323S1",
  "P3323S2",
  "P3323S3",
  "P3323S4",
  "P3323S5",
  "P3323S6",
  "P3324S1",
  "P3324S2",
  "P3324S3",
  "P3324S4",
  "P3325S1",
  "P3325S2",
  "P3325S3",
  "P3325S4",
  "P3326S1",
  "P3326S2",
  "P3326S3",
  "P3326S4",
  "P3326S5",
  "P3326S6",
  "P3326S7",
  "P3326S8",
  "P3326S9",
  "P3327S1",
  "P3327S2",
  "P3327S3",
  "P522",
  "P523",
  "P525",
  "P526",
  "P528",
  "P541",
  "P5501",
  "P564",
  "P565S1",
  "P565S2",
  "P565S3",
  "P565S4",
  "P568",
  "P577",
  "vic"
)]


haven::write_dta(dt_p_pj,'dt_p_pj.dta')
saveRDS(dt_p_pj,'dt_p_pj.rds')
openxlsx::write.xlsx(dt_p_pj,'dt_p_pj.xlsx')

haven::write_dta(dt_pj_rt_p,'dt_pj_rt_p.dta')
saveRDS(dt_pj_rt_p,'dt_pj_rt_p.rds')
openxlsx::write.xlsx(dt_pj_rt_p,'dt_pj_rt_p.xlsx')

dt_p_pj$countd <- dt_p_pj$count/dt_p_pj$count
dt_p_pj$countd[is.na(dt_p_pj$countd) == TRUE] <- 0
table(dt_p_pj$P1343, dt_p_pj$countd)

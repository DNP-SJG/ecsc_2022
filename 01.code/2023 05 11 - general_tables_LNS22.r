# ------------------------------------------------------------------------------------------------ #
# INDIVIDUALS, HOUSEHOLDS AND CRIME FRAMES - ECSC 2022
#  2023/05/10
# ------------------------------------------------------------------------------------------------ #

library(haven) # Stata file loading/saving
library(openxlsx)  # Excel file loading/saving
library(Microsoft365R) # One drive login
library(AzureGraph) # One drive read
library(dplyr) # Collapsing and data management
library(tidyr) # Data management
library(ggplot2) #Plots

# 00. LOADS  AND MERGES CHAPTERS -------------------------------------------------------------------

# od <- get_business_onedrive()
# od$list_files()

## 00.01 Loads tables ------------------------------------------------------------------------------

# Files rename - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#
# Renaming done locally, renamed files were then uploaded to google drive. The main repo loads
# all tables from the online path.

# wd <- "L:/01.ecsc2022/02.enj2022/00.enj_data/00.tables"
# 
# setwd(paste0(wd,"/01.dta")); cap_names <- as.vector(list.files())
# setwd(paste0(wd,"/02.aux_tables"))
# 
# cap_lab <- as.data.frame(openxlsx::read.xlsx("cap_names.xlsx"))
# 
# setwd(paste0(wd,"/01.dta")); file.rename(cap_names, cap_lab); rm(cap_lab)
# cap_names <- as.vector(list.files())

# Loads and names files into a list - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#
pat <- "https://docs.google.com/uc?id=%s&export=download"
files <- openxlsx::read.xlsx(sprintf(pat, "1P4x_BW5PZz8HJP5KfznhVV7vWZ_FEqKd"))
cap_lab <- as.vector(files$cap)

# loading function
#
files_f <- function(x){haven::read_dta(sprintf(pat,x)) }
cap_list <- lapply(files$id_cap,files_f)

lapply(cap_list, function(x){paste0("Rows = ", nrow(x) , " / ", "Cols = ", ncol(x))})

names(cap_list) <- files$cap
# names(cap_list[["cg.dta"]])

# Creates a variable list for each chapter- - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#  
var_list <- data.frame(var = NA, cap = NA)

for(i in 1:length(files$cap)){
  aux <- data.frame(var = names(cap_list[[files$cap[i]]]),cap = files$cap[i] )
  var_list <- rbind(var_list, aux)
  rm(aux)
  var_list <- var_list[is.na(var_list$var) == F,]
};rm(i)

#setwd(paste0(wd,"/02.aux_tables"));list.files()
#var_lab <- openxlsx::read.xlsx('2023 04 208 - diccionario III de datos ECSC 2022.xlsx')
var_lab <- openxlsx::read.xlsx(sprintf(pat, "1S5Arq1UiexCIYFdB_Ldj768D0XFpfMBi"))

var_lab$id <- paste0(var_lab$var,var_lab$var_lab)
table(duplicated(var_lab$id))
var_lab <- var_lab[!duplicated(var_lab$id),]

table(var_list$var %in% var_lab$var);table(var_lab$var %in% var_list$var) 

var_list <- merge(var_list,var_lab, all.x = TRUE, by = 'var')
#openxlsx::write.xlsx(var_list, 'var_list.xlsx')

rm(files_f,var_lab,files)

## 00.02 individual level  tables ------------------------------------------------------------------

# Socio demographic tables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 
cat(cap_lab)
cg <- cap_list[['cg.dta']]
cg_cierre <- cap_list[['cg_cierre.dta']]

cg$keyp <- paste0(cg$DIRECTORIO,
                  cg$SECUENCIA_ENCUESTA,
                  cg$SECUENCIA_P,
                  cg$ORDEN)
cg_cierre$keyp <- paste0(cg_cierre$DIRECTORIO,
                         cg_cierre$SECUENCIA_ENCUESTA,
                         cg_cierre$SECUENCIA_P,
                         cg_cierre$ORDEN)

table(duplicated(cg$keyp));table(duplicated(cg_cierre$keyp))
table(cg_cierre$keyp %in% cg$keyp)
table(cg$keyp %in% cg_cierre$keyp)
table(cg$keyp %in% cg_cierre$keyp,cg$P5785)

# 122.251 total records in the socio demographic table, of which 95.388 have records in the in-depth 
# demographics module (26.863 with no data, all from individuals between 0 and 15 years).
# 

dt_p <- cg
table(names(dt_p)%in%names(cg_cierre))

print(names(dt_p)[names(dt_p)%in%names(cg_cierre) == F])
print(names(dt_p)[names(dt_p)%in%names(cg_cierre) == T])

print(names(cg_cierre)[names(cg_cierre)%in%names(dt_p) == F])
print(names(cg_cierre)[names(cg_cierre)%in%names(dt_p) == T])

cg_cirrenames <- names(cg_cierre)[names(cg_cierre)%in%names(dt_p) == F]
dt_p <- merge(dt_p,cg_cierre[,c('keyp',cg_cirrenames)], all.x = TRUE, by = 'keyp')

table(is.na(dt_p$P3039));rm(cg_cirrenames,cg_cierre,cg)

# Household tables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 
cat(cap_lab)
dv <- cap_list[['dv.dta']]

dv$keyh <- paste0(dv$DIRECTORIO)
dt_p$keyh <- paste0(dt_p$DIRECTORIO)

sum(dv$FEX_C[!duplicated(dv$keyh)])
sum(dt_p$FEX_C[!duplicated(dt_p$DIRECTORIO)])

# The total amount of households, conditional on the uniqueness of the DIRECTORIO identifier, both
# in the CV and CG tables is 17,052,679 that is different from DANE report which identifies
# 17,108,863.956

table(duplicated(dv$keyh));table(duplicated(dt_p$keyh))

table(dv$keyh %in% dt_p$keyh)
table(dt_p$keyh %in% dv$keyh)

dv_names <- names(dv)[names(dv)%in%names(dt_p) == F]
dt_p <- merge(dt_p,dv[,c('keyh',dv_names)], all.x = TRUE, by = 'keyh')

table(is.na(dt_p$P1987));rm(dv_names,dv)

# Security perception tables   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 
cat(cap_lab)
percepcion_seguridad <- cap_list[['percepcion_seguridad.dta']]

percepcion_seguridad$keyp <- paste0(percepcion_seguridad$DIRECTORIO,
                                    percepcion_seguridad$SECUENCIA_ENCUESTA,
                                    percepcion_seguridad$SECUENCIA_P,
                                    percepcion_seguridad$ORDEN)

table(duplicated(percepcion_seguridad$keyp));table(duplicated(dt_p$keyp))

table(percepcion_seguridad$keyp %in% dt_p$keyp)
table(dt_p$keyp %in% percepcion_seguridad$keyp)

percepcion_seguridad_names <- names(percepcion_seguridad)[names(percepcion_seguridad)%in%names(dt_p) == F]
dt_p <- merge(dt_p,percepcion_seguridad[,c('keyp',percepcion_seguridad_names)], all.x = TRUE, by = 'keyp')

table(is.na(dt_p$P3105));rm(percepcion_seguridad_names,percepcion_seguridad)

# crime filter table - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 
cat(cap_lab)
filtro_delitos <- cap_list[['filtro_delitos.dta']]

filtro_delitos$keyp <- paste0(filtro_delitos$DIRECTORIO,
                              filtro_delitos$SECUENCIA_ENCUESTA,
                              filtro_delitos$SECUENCIA_P,
                              filtro_delitos$ORDEN)

table(duplicated(dt_p$keyp));table(duplicated(filtro_delitos$keyp))
table(filtro_delitos$keyp %in% dt_p$keyp)
table(dt_p$keyp %in% filtro_delitos$keyp)
table(dt_p$keyp %in% filtro_delitos$keyp,dt_p$P5785)

# 122.251 total records in the socio demographic table, of which 95.388 have records in the crime 
# filter module (26.863 with no data, all from individuals between 0 and 15 years).
# 

filtro_delitos_names <- 
  names(filtro_delitos)[names(filtro_delitos)%in%names(dt_p) == F]

dt_p <- merge(dt_p,filtro_delitos[,c('keyp',filtro_delitos_names)], 
              all.x = TRUE, 
              by = 'keyp')

table(is.na(dt_p$P541));rm(filtro_delitos_names,filtro_delitos)

# Contribution to security table - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 
cat(cap_lab)
percepcion_aporte <- as.data.frame(cap_list[['percepcion_aporte.dta']])

percepcion_aporte$keyp <- paste0(percepcion_aporte$DIRECTORIO,
                                 percepcion_aporte$SECUENCIA_ENCUESTA,
                                 percepcion_aporte$SECUENCIA_P,
                                 percepcion_aporte$ORDEN)

table(duplicated(dt_p$keyp));table(duplicated(percepcion_aporte$keyp))
table(percepcion_aporte$keyp %in% dt_p$keyp)
table(dt_p$keyp %in% percepcion_aporte$keyp)
table(dt_p$keyp %in% percepcion_aporte$keyp,dt_p$P5785)

# 122.251 total records in the socio demographic table, of which 95.388 have records in the security 
# contribution module (26.863 with no data, all from individuals between 0 and 15 years).
# 

percepcion_aporte_names <- 
  names(percepcion_aporte)[names(percepcion_aporte)%in%names(dt_p) == F]

dt_p <- merge(dt_p,percepcion_aporte[,c('keyp',percepcion_aporte_names)], 
              all.x = TRUE, 
              by = 'keyp')

table(is.na(dt_p$P1182S3));rm(percepcion_aporte,percepcion_aporte_names)


# 01. REPLICATES DANE PUBLISHED RECORDS ------------------------------------------------------------

# od <- get_business_onedrive()
# od$list_files()

## 01.01 Households -------------------------------------------------------------------------------- 
# Households records are not matching: available records on the published micro data are less than 
# the aggregate data presented in the official release (17,052,679 us VS 17,108,863 them). Records
# on the published household tables are unique, so the household ID dropping records should not be
# a concern.
# 
sum(dt_p$FEX_[!duplicated(dt_p$DIRECTORIO)])
dt_p[!duplicated(dt_p$DIRECTORIO),] %>% group_by(Clase) %>% summarise(SUM = sum(FEX_C))

sum(dt_p$FEX_[!duplicated(dt_p$DIRECTORIO)  & dt_p$P1987 == 1 ])
sum(dt_p$FEX_[!duplicated(dt_p$DIRECTORIO)  & dt_p$P1987 >  2])

## 01.02. Age -------------------------------------------------------------------------------------- 
# 
dt_p %>% group_by(P5785, P220) %>% summarise(SUM = sum(FEX_C))
dt_p %>% ggplot(aes(x = P5785, fill = as.factor(P220), weigth = FEX_C)) + 
  geom_histogram() +
  facet_grid(as.factor(P220) ~ .) + 
  guides(fill = FALSE)
sum(dt_p$FEX_[!duplicated(dt_p$keyp) & dt_p$P5785 > 14])

## 01.03. Class ------------------------------------------------------------------------------------
#
dt_p[dt_p$P5785 > 14,] %>% group_by(Clase, P220) %>% summarise(SUM = sum(FEX_C))

dt_p %>% ggplot(aes(x = P5785, fill = as.factor(Clase), weigth = FEX_C)) + 
  geom_histogram() +
  facet_grid(as.factor(Clase) ~ .,scales = "free") + 
  guides(fill = FALSE)

as.factor(cut(dt_p$P5785,breaks = 6)
)
## 01.04. Crime filter -----------------------------------------------------------------------------

# Victimizacion (hurto a personas - P1343
#                 hurto a vehículos - P1179
#                 hurto a bicicletas - P1179
#                 hurto a residencias - P1392
#                 hurto de ganado, semovientes o aves de corral - P1960
#                 riñas y peleas - P1315
#                 extorsión - P1286
#                 incidentes de seguridad digital - NA) - - - - - - - - - - - - - - - - - - - - - - 


vic_vars <- c("P1343","P1179","P1392","P1960","P1315","P1286" )

dt_p1 <- dt_p
lapply(vic_vars,function(x){ table(dt_p1[,x])})
lapply(vic_vars,function(x){ table(is.na(dt_p1[,x]))})

dt_p1 <- dt_p1 %>% mutate_at(vic_vars, ~ replace_na(.,0)) # 0 as NA in victimization records
dt_p1[ , vic_vars ][ dt_p1[ , vic_vars ] == 2 ] <- 0 # recode 2 as 0 to get general victimization
dt_p <- dt_p %>% mutate_at('P577', ~ replace_na(.,0)); table(dt_p$P577) # recode double report veh

dt_p$vic <- rowSums(dt_p1[,vic_vars], na.rm = TRUE); table(dt_p$vic)
dt_p$vic <- dt_p$vic/dt_p$vic

dt_p[dt_p$P577 != 1 ,] %>% group_by(Clase,vic) %>% summarise(SUM = sum(FEX_C,na.rm = TRUE))
dt_p[dt_p$P577 != 1 ,] %>% group_by(vic) %>% summarise(SUM = sum(FEX_C,na.rm = TRUE))

rm(dt_p1)

# Rinas  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#
table(is.na(dt_p$P1315))
dt_p[dt_p$P5785 > 14,] %>% group_by(Clase, P1315) %>% summarise(SUM = sum(FEX_C,na.rm = TRUE))

rm(dt_p1,vic_vars)

# Hurto a personas - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#
table(is.na(dt_p$P1343))


dt_p[dt_p$P577 != 1,] %>% group_by(Clase, P1343) %>% summarise(SUM = sum(FEX_C,na.rm = TRUE))
dt_p[dt_p$P577 != 1,] %>% group_by( P1343) %>% summarise(SUM = sum(FEX_C,na.rm = TRUE))

# Hurto a vehiculos  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -s
#

dt_p <- dt_p %>% mutate_at('P568', ~ replace_na(.,0)); table(dt_p$P568)
table(is.na(dt_p$P1179))
table(is.na(dt_p$P565S3))

dt_p <- dt_p %>% mutate_at('P565S3', ~ replace_na(.,0)); table(dt_p$P565S3)

dt_p[dt_p$P565S3 != 1,] %>% group_by(P1179) %>% summarise(SUM = sum(FEX_C,na.rm = TRUE))
dt_p %>% group_by( P1179) %>% summarise(SUM = sum(FEX_C,na.rm = TRUE))

#
dt_p %>% group_by(P1392) %>% summarise(SUM = sum(FEX_C,na.rm = TRUE))
dt_p %>% group_by( P1392, Clase) %>% summarise(SUM = sum(FEX_C,na.rm = TRUE))

# Hurto a animales  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#
dt_p %>% group_by(P1960) %>% summarise(SUM = sum(FEX_C,na.rm = TRUE))
dt_p %>% group_by( P1960, Clase) %>% summarise(SUM = sum(FEX_C,na.rm = TRUE))

# Extorsion  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#
dt_p %>% group_by(P1286) %>% summarise(SUM = sum(FEX_C,na.rm = TRUE))
dt_p %>% group_by( P1286, Clase) %>% summarise(SUM = sum(FEX_C,na.rm = TRUE))

# 02. SAVES GENERAL INDIVIDUALS, HOUSEHOLDS AND CRIME FRAME ----------------------------------------

wd <- "L:/01.ecsc2022/02.enj2022/00.enj_data/00.tables/01.out_dta"
setwd(wd)
saveRDS(dt_p, 'dt_p.rds')
haven::write_dta(dt_p, 'dt_p.dta', version = 15 )
haven::write_dta()


# ------------------------------------------------------------------------------------------------ #
# PROBLEM AND LEGAL NEEDS FRAME - ECSC 2022
#  2023/05/11
# ------------------------------------------------------------------------------------------------ #

library(haven) # Stata file loading/saving
library(openxlsx)  # Excel file loading/saving
library(Microsoft365R) # One drive login
library(AzureGraph) # One drive read
library(dplyr) # Collapsing and data management
library(tidyr) # Data management
library(ggplot2) #Plots
library(stats)
library(stringr) # leading zeros and other string work

# 00. LOADS  AND MERGES CHAPTERS -------------------------------------------------------------------

# od <- get_business_onedrive()
# od$list_files()

## 00.01 Loads tables ------------------------------------------------------------------------------

# Loads and names files into a list - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#
pat <- "https://docs.google.com/uc?id=%s&export=download"
files <- openxlsx::read.xlsx(sprintf(pat, "1P4x_BW5PZz8HJP5KfznhVV7vWZ_FEqKd"))
cap_lab <- as.vector(files$cap)

# loading function ECSC chapters
#
files_f <- function(x){haven::read_dta(sprintf(pat,x)) }
cap_list <- lapply(files$id_cap,files_f)

lapply(cap_list, function(x){paste0("Rows = ", nrow(x) , " / ", "Cols = ", ncol(x))})

names(cap_list) <- files$cap
# names(cap_list[["cg.dta"]])

# Loads consolidated individuals, households and crime frame - - - - - - - - - - - - - - - - - - - -

gdrive_shared <- "https://drive.google.com/file/d/1KUwutTOQrI5crMfQCqTWRgi4wFLk2b4P/view"

dt_p <- usethis::create_download_url(gdrive_shared) |>
  url() |>
  readRDS() |>
  tibble::as_tibble()
rm(gdrive_shared)

# Loads problem tables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 names(cap_list); rm(files_f())
 
 pdcd_largo_p <- cap_list[['pdcd_largo_p.dta']]
 pdcd_largo_sinp <- cap_list[['pdcd_largo_sinp.dta']]
 pdcd_corto <- cap_list[['pdcd_corto.dta']]
 
 # 01. PROBLEM DECLARATION FRAME -------------------------------------------------------------------
 
 ## 01.1 Priority long cycle -----------------------------------------------------------------------
 pdcd_largo_p$keyp <- paste0( pdcd_largo_p$DIRECTORIO,
                              pdcd_largo_p$SECUENCIA_ENCUESTA,
                              pdcd_largo_p$SECUENCIA_P,
                              pdcd_largo_p$ORDEN )
 
 # sum(pdcd_largo_p$FEX_C[!duplicated(pdcd_largo_p$keyp)]) / sum(dt_p$FEX_C[!duplicated(dt_p$keyp)])  
 
 # All individual records in the priority long cycle (i.e. the main declaration table) are identified
 # in the individual, households and crime table (dt_p). The pdcd_largo_p has a wide format with  
 # every recorded problem occurrence recorded as a variable. More over, the table groups information  
 # for justice routes for all prioritized problems.
 # 
 
 # Before going in depth with the justice routes data, there is a need for a individual-problem frame
 # to do so, a long format, rather than a wide table is needed.
 #  
 
  table(!duplicated(pdcd_largo_p$keyp))
  table(pdcd_largo_p$keyp %in% dt_p$keyp) 
 
  table(pdcd_largo_p$P3013)
  
  # Problem type reshape (P1670S Pcat A1_ Ptype) - - - - - - - - - - - - - - - - - - - - - - - - - - 
 
  # table format should be shape by problem typologies, categories can be build later.
  
  length(grep('A1_', names(pdcd_largo_p)))
  var_t <- names(pdcd_largo_p)[grep('A1_', names(pdcd_largo_p))]
  
  lapply(var_t,function(x){ table(pdcd_largo_p[,x])})
  lapply(var_t,function(x){ table(is.na(pdcd_largo_p[,x]))}) 
  pdcd_largo_p <- pdcd_largo_p|>mutate_at(var_t, ~ replace_na(.,0)) # 0 as NA problem records
  
  # nj_count1 > total number of declared and classified problems
  #
  
  pdcd_largo_p$nj_count1 <- rowSums(pdcd_largo_p[,var_t],na.rm = TRUE)
  
  # Wide to long on problem type
  # 
  dt_ln1 <- tidyr::gather(pdcd_largo_p[c('keyp','FEX_C',var_t)], 
                          p_type, 
                          nj_count1, 
                          var_t, 
                          factor_key = TRUE)
  
  dt_ln1 <-   dt_ln1[which(dt_ln1$nj_count1 != 0),]

# Problem month reshape (P3009 S1_ PCat_Ptype ) - - - - - - - - - - - - - - - - - - - - - - - - - - 
  
  length(grep('P3009S1_', names(pdcd_largo_p)))
  var_m <- names(pdcd_largo_p)[grep('P3009S1_', names(pdcd_largo_p))]
  
  # Wide to long on month
  # 
  dt_mn1 <- tidyr::gather(pdcd_largo_p[c('keyp','FEX_C',var_m)], 
                          month_q, 
                          month, 
                          var_m, 
                          factor_key = TRUE)
  
  dt_mn1 <- dt_mn1[is.na(dt_mn1$month) == FALSE,]
  table(dt_mn1$month)
  
 # Problem year reshape (P3009 S2_ PCat_Ptype ) - - - - - - - - - - - - - - - - - - - - - - -  - - -
  
  length(grep('P3009S2_', names(pdcd_largo_p)))
  var_y <- names(pdcd_largo_p)[grep('P3009S2_', names(pdcd_largo_p))]
  
  # Wide to long on year
  # 
  dt_ye1 <- tidyr::gather(pdcd_largo_p[c('keyp','FEX_C',var_y)], 
                          year_q, 
                          year, 
                          var_y, 
                          factor_key = TRUE)
  
  dt_ye1 <- dt_ye1[is.na(dt_ye1$year) == FALSE,]
  table(dt_ye1$year)
  
 # Problem impact reshape (P3009 S2_ PCat_Ptype ) - - - - - - - - - - - - - - - - - - - - - - - - - 
  #
  
  length(grep('P3011_', names(pdcd_largo_p)))
  var_i <- names(pdcd_largo_p)[grep('P3011_', names(pdcd_largo_p))]
  
  # Wide to long on impact
  # 
  dt_im1 <- tidyr::gather(pdcd_largo_p[c('keyp','FEX_C',var_i)], 
                          impact_q, 
                          impact, 
                          var_i, 
                          factor_key = TRUE)
  
  dt_im1 <- dt_im1[is.na(dt_im1$impact) == FALSE,]
  table(dt_im1$impact)
  
### 01.1.1 individual-problem key ------------------------------------------------------------------
  # Problems 6 -9 and 11 - 8 were not reported at all.
  #  
  
  sum(dt_ln1$FEX_C[ grep('P1670S14A1_', dt_ln1$p_type) ])
  sum(dt_ln1$FEX_C[ grep('P1670S10A1_', dt_ln1$p_type) ])
  
  # openxlsx::write.xlsx(dt_ln1[!duplicated(dt_ln1$p_type),], 'dt_ln1_pty.xlsx')
  # openxlsx::write.xlsx(dt_im1[!duplicated(dt_im1$impact_q),], 'dt_im1_pty.xlsx')
  # openxlsx::write.xlsx(dt_ye1[!duplicated(dt_ye1$year_q),], 'dt_ye1_pty.xlsx')
  # openxlsx::write.xlsx(dt_mn1[!duplicated(dt_mn1$month_q),], 'dt_month_pty.xlsx')
  
  var_i%in%dt_im1$impact_q[!duplicated(dt_im1$impact_q)]
  
  # Loads records with type code - varible link. This new variables will constitude the 
  # individual-type key for the problem frame.
  # 
  dt_ln1labs <- openxlsx::read.xlsx(sprintf(pat, "1_QSyAz-Cuoiq2TlVJuQsvQ55oz7A5FN6"))
  dt_im1labs <- openxlsx::read.xlsx(sprintf(pat, "1dbzIYubAVEra07t5rRrLQ85HAJRGe0tg"))
  dt_mn1labs <- openxlsx::read.xlsx(sprintf(pat, "1XuN7CwzNhhznP7h9UYjdEoBzMEOl-rYn"))
  dt_ye1labs <- openxlsx::read.xlsx(sprintf(pat, "1KlBvUvsy9nvNdUHTu026zuWZz_-9q2R0"))

  dt_ln1 <- merge(dt_ln1, dt_ln1labs, all.x = TRUE, by = 'p_type')
  dt_im1 <- merge(dt_im1, dt_im1labs[,c('impact_q','type_id')], all.x = TRUE, by = 'impact_q')
  dt_ye1 <- merge(dt_ye1, dt_ye1labs[,c('year_q','type_id')], all.x = TRUE, by = 'year_q')
  dt_mn1 <- merge(dt_mn1, dt_mn1labs[,c('month_q','type_id')], all.x = TRUE, by = 'month_q')
  
  temp_dt <- list(dt_mn1,dt_ye1,dt_im1,dt_ln1)
  names(temp_dt) <- c('dt_mn1','dt_ye1','dt_im1','dt_ln1')
  
  rm(dt_mn1,dt_ye1,dt_im1,dt_ln1);rm(dt_ln1labs,dt_im1labs,dt_mn1labs,dt_ye1labs);rm(var_lab)
  rm(var_i,var_m,var_t,var_t,var_y)
  
  temp_dt <- lapply(temp_dt, function(x){cbind(x, keypp = paste0(x[,'keyp'], x[,'type_id']))})

### 01.1.2 individual-problem frame ----------------------------------------------------------------
  # Creates single problem-individual frame, merging type, impact and date
  #
  
  lapply(temp_dt, function(x){table(x[,'keypp'] %in% dt_pj$keypp)}) # all keypp present in dt_pj
  
  temp_dt[['dt_im1']]$keyp <- NULL
  temp_dt[['dt_ye1']]$keyp <- NULL
  temp_dt[['dt_mn1']]$keyp <- NULL

  dt_pj <- Reduce(inner_join, temp_dt)
  dt_pj <- arrange(dt_pj, keypp)
  
  # re-orders varibles
  #
  #names(dt_pj)
  dt_pj <- dt_pj[,c('keyp','keypp','FEX_C','cat_lab','cat_labs','cat_en','cat_id',
                    'type_lab','type_labs','type_en','type_id','impact','year','month',
                    'p_type','impact_q','year_q','month_q','nj_count1')]

### Creates variable to identify the problems under priority - - - - - - - - - - - - - - - - - - - -
  
  # P3013 priority one
  # P3014 priority two
  
  pdcd_largo_p$P3013 <- (str_pad(pdcd_largo_p$P3013, 4, pad = "0"))
  pdcd_largo_p$P3014 <- (str_pad(pdcd_largo_p$P3014, 4, pad = "0"))
  
  table(pdcd_largo_p$P3013); table(is.na(pdcd_largo_p$P3013))
  table(pdcd_largo_p$P3014); table(is.na(pdcd_largo_p$P3014))
  
  pdcd_largo_p$nj_prio <- 1
  pdcd_largo_p$keyppp1 <- paste0(pdcd_largo_p$keyp, pdcd_largo_p$P3013)
  pdcd_largo_p$keyppp2 <- paste0(pdcd_largo_p$keyp, pdcd_largo_p$P3014)
  
  dt_pj <- merge(dt_pj,pdcd_largo_p[,c('keyppp1','nj_prio')], 
                 all.x = TRUE, 
                 by.x = 'keypp', 
                 by.y = 'keyppp1')
  dt_pj <- merge(dt_pj,pdcd_largo_p[,c('keyppp2','nj_prio')],
                 all.x = TRUE, 
                 by.x = 'keypp', 
                 by.y = 'keyppp2')
  
  dt_pj$nj_prio <- rowSums(dt_pj[c('nj_prio.x', 'nj_prio.y')], na.rm = TRUE) 
  dt_pj$nj_prio.x <- NULL
  dt_pj$nj_prio.y <- NULL
  
  
  # At this stage the ** nj_prio ** variable only holds records from declarants with three or more
  # problems. By construction, all problems declared by individualas with as much as 2 problems
  # are considered to hold priority status. Thus, it is needed to either merge the problem ID from 
  # the ** pdcd_largo_sp ** table, or build a count varaible in the ** dt_pj ** table.
  # 
  
### Identifies priority status for declarents with 2<= declaration - - - - - - - - - - - - - - - - -
  
  dt_pj <- dt_pj %>% group_by(keyp) %>% mutate(nj_count1 = n())
  dt_pj$nj_prio[dt_pj$nj_count1 <=2] <- 1
  
  table(dt_pj$nj_prio,dt_pj$nj_count1 );hist(tally(group_by(dt_pj, keyp))$n)
  
  # 02. PROBLEM DECLARATION FRAME -------------------------------------------------------------------
  
  ## 01.1 Priority long cycle -----------------------------------------------------------------------
  
  
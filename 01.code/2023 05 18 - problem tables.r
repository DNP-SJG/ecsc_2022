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

# Sets simplified key
dt_p$keyp <- paste0( dt_p$DIRECTORIO,
                     dt_p$SECUENCIA_P,
                     dt_p$ORDEN)

table(duplicated(dt_p$keyp))

# Loads problem tables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 names(cap_list)
 
 pdcd_largo_p <- cap_list[['pdcd_largo_p.dta']]
 pdcd_largo_sinp <- cap_list[['pdcd_largo_sinp.dta']]
 pdcd_corto <- cap_list[['pdcd_corto.dta']]
 
 # 01. PROBLEM DECLARATION FRAME -------------------------------------------------------------------
 
 ## 01.1 Priority long cycle -----------------------------------------------------------------------
 pdcd_largo_p$keyp <- paste0( pdcd_largo_p$DIRECTORIO,
                              pdcd_largo_p$SECUENCIA_P,
                              pdcd_largo_p$ORDEN )
 
 # sum(pdcd_largo_p$FEX_C[!duplicated(pdcd_largo_p$keyp)]) / sum(dt_p$FEX_C[!duplicated(dt_p$keyp)])  
 
 # All individual records in the priority long cycle (i.e. the main declaration table) are identified
 # in the individual, households and crime table (dt_p). The pdcd_largo_p has a wide format with  
 # every problem occurrence recorded as a variable. Moreover, the table groups information  
 # for justice routes for all prioritized problems under >=3 declaration.
 # 
 
 # Before going in depth with the justice routes data, there is a need for a individual-problem frame
 # to do so, a long format, rather than a wide table is needed.
 #  
  table(!duplicated(pdcd_largo_p$keyp))
  table(pdcd_largo_p$keyp %in% dt_p$keyp) 
  table(pdcd_largo_p$P3013)
  
  # Problem type reshape (P1670S Pcat A1_ Ptype) - - - - - - - - - - - - - - - - - - - - - - - - - - 
 
  # table format should be shape by problem typologies, categories can be build later.
  #
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
#
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
 #
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

  temp_dt <- lapply(temp_dt, function(x){cbind(x, keypp = paste0(x[,'keyp'], x[,'type_id']))})

### 01.1.2 individual-problem frame ----------------------------------------------------------------
  # Creates single problem-individual frame, merging type, impact and date
  #
  lapply(temp_dt, function(x){table(x[,'keypp'] %in% dt_ln1$keypp)}) # all keypp present in dt_ln1
  
  rm(dt_mn1,dt_ye1,dt_im1,dt_ln1);rm(dt_ln1labs,dt_im1labs,dt_mn1labs,dt_ye1labs);rm(var_lab)
  rm(var_i,var_m,var_t,var_t,var_y)
  
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
  
# 02. PROBLEM ROUTES -------------------------------------------------------------------------------
  
  ## 02.1. Institutional route ---------------------------------------------------------------------
  ### 02.1.1. Priority long cycle ------------------------------------------------------------------
  #
  length(grep('A1_', names(pdcd_largo_p)))
  var_t  <- names(pdcd_largo_p)[grep('A1_', names(pdcd_largo_p))] #typologies
  var_r  <- names(pdcd_largo_p)[grep('P1672_', names(pdcd_largo_p))] #routes
  
  #### 02.1.1.1 route table -------------------------------------------------------------------------
  #
  dt_route1 <- pdcd_largo_p[is.na(pdcd_largo_p$P3013) == FALSE, 
                            c('keyp','keyppp1','P3013','P1672_1')]
  
  dt_route2 <- pdcd_largo_p[is.na(pdcd_largo_p$P3014) == FALSE, 
                            c('keyp','keyppp2','P3014','P1672_2')]
  
  names(dt_route1)[names(dt_route1) == 'keyppp1'] <- 'keypp'
  names(dt_route1)[names(dt_route1) == 'P3013']   <- 'type_id'
  names(dt_route1)[names(dt_route1) == 'P1672_1']   <- 'P1672'
  names(dt_route2) <-  names(dt_route1)

  dt_route <- rbind(dt_route1,dt_route2);rm(dt_route1,dt_route2)
  table(is.na(dt_route$P1672)); table((dt_route$P1672))
  

  var_i11 <- names(pdcd_largo_p)[grep('P1673S', names(pdcd_largo_p))[1:41]]  #institutions all visited
  var_i12 <- names(pdcd_largo_p)[grep('P1673S', names(pdcd_largo_p))[42:82]] #institutions all visited
  var_i2 <- names(pdcd_largo_p)[grep('P1674_', names(pdcd_largo_p))] #institutions last visited
  
  var_ir <- names(pdcd_largo_p)[grep('P1675_', names(pdcd_largo_p))] #institutional route reason
  var_irr <- names(pdcd_largo_p)[grep('P1676', names(pdcd_largo_p))] #institutional route results
  
  var_irnr <- names(pdcd_largo_p)[grep('P1677', names(pdcd_largo_p))] #resons for no result
  var_irnc <- names(pdcd_largo_p)[grep('P1678', names(pdcd_largo_p))] #reasons for not continuing
  
  # institutions, alongside types, are the only wide strcuture in the table. Thus, it is needed
  # to set the addecuated long format for thos problems properly set by priority order 2 
  # (i.e. declaraction >= 3). There is a total of 1,249 of such records (linking only to problems
  # faced using the institutional rout) under priority order 2.
  #
  # Institutions visited (** institution_allq ** ) for priority one
  # 
  dt_inst_routPLC1 <- tidyr::gather(pdcd_largo_p[is.na(pdcd_largo_p$P3013) == FALSE,
                                                 c('keyp','keyppp1','P3013',var_i11)], 
                          institution_allq, 
                          nj_count1, 
                          var_i11, 
                          factor_key = TRUE)
  
  dt_inst_routPLC1 <-   dt_inst_routPLC1[is.na(dt_inst_routPLC1$nj_count1) == FALSE,]
  
  table(duplicated(dt_inst_routPLC1$keyppp1))
  
  # Institutions visited (** institution_allq ** ) for priority two
  # 
  dt_inst_routPLC2 <- tidyr::gather(pdcd_largo_p[is.na(pdcd_largo_p$P3014) == FALSE,
                                                 c('keyp','keyppp2','P3014',var_i12)], 
                                    institution_allq, 
                                    nj_count1, 
                                    var_i12, 
                                    factor_key = TRUE)

  dt_inst_routPLC2 <-   dt_inst_routPLC2[is.na(dt_inst_routPLC2$nj_count1) == FALSE,]
  table(duplicated(dt_inst_routPLC2$keyppp2))
  
  # Rbind long institution table for each priority problem.
  # 
  names(dt_inst_routPLC2)[names(dt_inst_routPLC2) == 'keyppp2'] <- 'keypp'
  names(dt_inst_routPLC2)[names(dt_inst_routPLC2) == 'P3014']   <- 'type_id'
  
  names(dt_inst_routPLC1) <- names(dt_inst_routPLC2)
  dt_inst_routPLC <- rbind(dt_inst_routPLC1,dt_inst_routPLC2);rm(dt_inst_routPLC1,dt_inst_routPLC2)
  
  dt_inst_routPLC$institution_allq <- as.character(dt_inst_routPLC$institution_allq)
  dt_inst_routPLC$institution_allq <- substr(dt_inst_routPLC$institution_allq,1,
                                            nchar(dt_inst_routPLC$institution_allq)-2)
  
  dt_instlabs <- openxlsx::read.xlsx(sprintf(pat, "1tB7MQUDs4TIgB7Wgoq18QlqiJ_xUaTjd"))
  dt_inst_routPLC <- merge(dt_inst_routPLC, dt_instlabs, all.x = TRUE, by = 'institution_allq')
  
  rm(var_i11,var_i12)
  
  ### 02.1.2. Priority long cycle ------------------------------------------------------------------
  
  sum(pdcd_largo_sinp$FEX_C)
  sum(dt_pj$FEX_C[dt_pj$nj_count1 < 3])
  
  # Possible issue with characterization? not all problems reported in the general declaration table
  # are recorded in the long cicle with no priority (4915632 vs 4931342). Yet all individual-problem
  # records in the long cycle no priority should be part of the declaration table.
  # 
  pdcd_largo_sinp$type_id <- pdcd_largo_sinp$P3154
  table((pdcd_largo_sinp$type_id)%in%(dt_pj$type_id))
  
  # In the long cycle with no priority the variables  SECUENCIA_ENCUESTA and ORDEN are exactly the
  # same, there is a need to find out the correct intendifyer.
  # 
  table(pdcd_largo_p$SECUENCIA_ENCUESTA)
  table(pdcd_largo_sinp$SECUENCIA_ENCUESTA)
  
  table(pdcd_largo_p$ORDEN)
  table(pdcd_largo_sinp$ORDEN)
  
  # Turns out, there is a faulty labaling in the variable labeling of the table **pdcd_largo_sinp**.
  # This  issue was making the unique identityer hard to get by. The solution forced us to 
  # simplify the individual key in the ** dt_p ** and ** dt_pj** tables, affecting also the 
  # problem-individual key.The varaible label correspondeces in table  ** pdcd_largo_sinp ** are as
  # follows
  # 
  # ___pdcd_largo_sinp____|__pdcd_largo_p____
  #    DIRECTORIO         |   DIRECTORIO     
  #    HOG                |   SECUENCIA_P    
  #    SECUENCIA_P        |   ORDEN          
  #

  # pdcd_largo_sinp$keyp <- paste0(pdcd_largo_sinp$DIRECTORIO,
  #                                pdcd_largo_sinp$HOG,
  #                                pdcd_largo_sinp$SECUENCIA_P)
  
  table(duplicated(pdcd_largo_sinp$keyp))
  
  pdcd_largo_sinp$keyp <- paste0(pdcd_largo_sinp$DIRECTORIO,
                                 pdcd_largo_sinp$HOG,
                                 pdcd_largo_sinp$SECUENCIA_P) 
  
  # 12.042 total problems in the long cycle no priority (declaration < 3)
  table(pdcd_largo_sinp$keyp %in% dt_pj$keyp)
  table(dt_pj$keyp %in% pdcd_largo_sinp$keyp)
  table(dt_pj$nj_count1)
  
  pdcd_largo_sinp$keypp <- paste0(pdcd_largo_sinp$keyp, pdcd_largo_sinp$type_id)
  table(pdcd_largo_sinp$keypp %in% dt_pj$keypp)
  
  
  #### 02.1.1.1 route table -------------------------------------------------------------------------
  
  table(pdcd_largo_sinp$P1672)
  dt_route_sinp <- pdcd_largo_sinp[c('keypp',
                                     'P1672',   # route
                                     'P1674',   # last institution
                                     'P1675',   # institution reason
                                     'P1676',   # institution result
                                     'P1676S2', # institution months
                                     'P1676S3', # institution satisfaction
                                     'P1677',   # instiution no result reason
                                     'P1678',   # institution drop out reason
                                     'P1679',   # autocomposition reason
                                     'P1680',   # autocomposition result
                                     'P1681',   # violence reason
                                     'P1682',   # Illegal actor reason
                                     'P1683',   # Inaction reason
                                     'P1684',   # Was the agreement met?
                                     'P1685',   # Was the problem solved
                                     'P1686',   # Time for solution
                                     'P1687',   # Path retaking
                                     'P1688',   # Reasons for not reataking
                                     'P1689',   # Legal assistance
                                     'P1690'   # Reasons for not legal assistance
                                     )]
  ## 02.2 No Priority long cycle -------------------------------------------------------------------
  ## 02.3 Short cycle ------------------------------------------------------------------------------
  

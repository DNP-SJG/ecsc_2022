# ------------------------------------------------------------------------------------------------ #
# PROBLEM AND LEGAL NEEDS FRAME - ECSC 2022
#  2023/05/11
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
  
  sum(dt_pj$FEX_C)
  dt_pj$id_dane <- 1

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
  
  dt_pj$nj_prio
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
  
  #### 02.1.1.1 Route table -------------------------------------------------------------------------
  #
  dt_route1 <- pdcd_largo_p[is.na(pdcd_largo_p$P3013) == FALSE, 
                            c('keyp',      # individual key
                              'keyppp1',   # individual problem key - priority order 1
                              'P3013',     # problem type - priority order 1
                              'P1672_1',   # route - priority order 1
                              'P1674_1',   # last institution
                              'P1675_1',   # institution reason
                              'P1676_1',   # institution result
                              'P1676S2_1', # institution months
                              'P1676S3_1', # institution satisfaction
                              'P1677_1',   # instiution no result reason
                              'P1678_1',   # institution drop out reason
                              'P1679_1',   # autocomposition reason
                              'P1680_1',   # autocomposition result
                              'P1681_1',   # violence reason
                              'P1682_1',   # illegal actor reason
                              'P1683_1',   # inaction reason
                              'P1684_1',   # was the agreement met?
                              'P1685_1',   # was the problem solved
                              'P1686_1',   # time for solution
                              'P1687_1',   # path retaking
                              'P1688_1',   # reasons for not reataking
                              'P1689_1',   # legal assistance
                              'P1690_1'    # reasons for not legal assistance
  )]
  
  dt_route2 <- pdcd_largo_p[is.na(pdcd_largo_p$P3014) == FALSE,
                            c('keyp',      # individual key
                              'keyppp2',   # individual problem key - priority order 2
                              'P3014',     # problem type - priority order 2
                              'P1672_2',   # route - priority order 2
                              'P1674_2',   # last institution
                              'P1675_2',   # institution reason
                              'P1676_2',   # institution result
                              'P1676S2_2', # institution months
                              'P1676S3_2', # institution satisfaction
                              'P1677_2',   # instiution no result reason
                              'P1678_2',   # institution drop out reason
                              'P1679_2',   # autocomposition reason
                              'P1680_2',   # autocomposition result
                              'P1681_2',   # violence reason
                              'P1682_2',   # illegal actor reason
                              'P1683_2',   # inaction reason
                              'P1684_2',   # was the agreement met?
                              'P1685_2',   # was the problem solved
                              'P1686_2',   # time for solution
                              'P1687_2',   # path retaking
                              'P1688_2',   # reasons for not reataking
                              'P1689_2',   # legal assistance
                              'P1690_2'    # reasons for not legal assistance
                            )]
  
  names(dt_route1) <- sub("_1", "", names(dt_route1))
  names(dt_route1)[names(dt_route1) == 'keyppp1'] <- 'keypp'
  names(dt_route2) <-  names(dt_route1)

  dt_route_p <- rbind(dt_route1,dt_route2);rm(dt_route1,dt_route2)
  table(is.na(dt_route_p$P1672)); table((dt_route_p$P1672))
  
  table(dt_route_p$keypp %in% dt_pj$keypp)
  
  #### 02.1.1.2. All visited institutions list -----------------------------------------------------
  
  var_i11 <- names(pdcd_largo_p)[grep('P1673S', names(pdcd_largo_p))[1:41]]  #institutions, all visited
  var_i12 <- names(pdcd_largo_p)[grep('P1673S', names(pdcd_largo_p))[42:82]] #institutions, all visited

  # institutions, alongside types, are the only wide strcuture in the table. Thus, it is needed
  # to set the addecuated long format for thos problems properly set by priority order 2 
  # (i.e. declaraction >= 3). There is a total of 1,249 of such records (linking only to problems
  # faced using the institutional rout) under priority order 2.
  #
  # Institutions visited (** institution_allq ** ) for priority one
  # 
  dt_inst_routPLC1 <- tidyr::gather(pdcd_largo_p[is.na(pdcd_largo_p$P3013) == FALSE,
                                                 c('keyp','keyppp1','P3013','P1672_1',var_i11)], 
                          institution_allq, 
                          nj_count1, 
                          var_i11, 
                          factor_key = TRUE)
  
  dt_inst_routPLC1 <-   dt_inst_routPLC1[is.na(dt_inst_routPLC1$nj_count1) == FALSE,]
  
  table(duplicated(dt_inst_routPLC1$keyppp1))
  
  # Institutions visited (** institution_allq ** ) for priority two
  # 
  dt_inst_routPLC2 <- tidyr::gather(pdcd_largo_p[is.na(pdcd_largo_p$P3014) == FALSE,
                                                 c('keyp','keyppp2','P3014','P1672_2',var_i12)], 
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
  
  rm(var_i11,var_i12,dt_inst_routPLC1, dt_inst_routPLC2)
  table(duplicated(dt_inst_routPLC$keypp))
  
  sum()
  ### 02.1.2. No priority long cycle ---------------------------------------------------------------
  
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
  pdcd_largo_sinp$keyp <- paste0(pdcd_largo_sinp$DIRECTORIO,
                                 pdcd_largo_sinp$HOG,
                                 pdcd_largo_sinp$SECUENCIA_P) 
  
  table(duplicated(pdcd_largo_sinp$keyp))
  table(pdcd_largo_sinp$keyp %in%pdcd_largo_p$keyp )
  
  pdcd_largo_sinp$keypp <- paste0(pdcd_largo_sinp$keyp,pdcd_largo_sinp$type_id)
  table(duplicated(pdcd_largo_sinp$keypp))
  table(pdcd_largo_sinp$keypp %in% dt_pj$keypp )
  
  pdcd_largo_sinp <- arrange(pdcd_largo_sinp, keypp)
  
  table(pdcd_largo_sinp$keyp %in%dt_pj$keyp )
  
  ## The wide structure of the priority long cycle does not allow for a single individual to report
  ## more than one problem with the same keyp-type_id combination. To solve this, the duplicated 
  ## records are going to be drop for the general routes consolidation, yet they will be appended
  ## to the final table once it is finished, in the no ** pdcd_largo_sinp ** there are 58 such 
  ## records..
  #
  duplicated_aux_sinp <- pdcd_largo_sinp$keypp[duplicated(pdcd_largo_sinp$keypp) == TRUE]
  # View(pdcd_largo_sinp[pdcd_largo_sinp$keypp %in% duplicated_aux_sinp == TRUE, ])
  # View(pdcd_largo_p[c("keyp",
  #                     "nj_count1",
  #                     "nj_prio",
  #                     "keyppp1",
  #                     "keyppp2",
  #                     'P1672_1',
  #                     'P1672_2')])
  duplicated_aux_sinp <- pdcd_largo_sinp[pdcd_largo_sinp$keypp %in% duplicated_aux_sinp == TRUE, ]
  duplicated_aux_sinp <- duplicated_aux_sinp[!duplicated(duplicated_aux_sinp$keypp),]
  #duplicated_aux_sinp <- duplicated_aux_sinp[duplicated_aux_sinp$ORDEN == 2,]
  table(duplicated_aux_sinp$ORDEN)
  
  pdcd_largo_sinp <- arrange(pdcd_largo_sinp, keypp, ORDEN)
  pdcd_largo_sinp <- pdcd_largo_sinp[!duplicated(pdcd_largo_sinp$keypp) ,]
  
  table(pdcd_largo_sinp$ORDEN[pdcd_largo_sinp$keypp %in% duplicated_aux_sinp$keypp ])
  table(duplicated(pdcd_largo_sinp$keypp))
  
  # 12.042 total problems in the long cycle no priority (declaration < 3)
  table(pdcd_largo_sinp$keyp %in% dt_pj$keyp)
  table(dt_pj$keyp %in% pdcd_largo_sinp$keyp)
  table(dt_pj$nj_count1)
  
  table(pdcd_largo_sinp$keypp %in% dt_pj$keypp)
  table(pdcd_largo_sinp$P1672)

  #### 02.1.2.1 Route table -------------------------------------------------------------------------
  
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
                                     'P1690'    # Reasons for not legal assistance
                                     )]
  
  dt_route_sinp_duplicated_aux <- duplicated_aux_sinp[c('keypp',
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
                                     'P1690'    # Reasons for not legal assistance
  )]

  #### 02.1.2.2. All visited institutions list -----------------------------------------------------
  
  # var_i11 <- names(pdcd_largo_sinp)[grep('P1673S', names(pdcd_largo_sinp))] 
  # 
  # # institutions, alongside types, are the only wide strcuture in the table. Thus, it is needed
  # # to set the addecuated long format for thos problems properly set by priority order 2 
  # # (i.e. declaraction >= 3). There is a total of 1,249 of such records (linking only to problems
  # # faced using the institutional rout) under priority order 2.
  # #
  # # Institutions visited (** institution_allq ** ) for priority one
  # # 
  # dt_inst_routNPLC <- tidyr::gather(pdcd_largo_sinp[is.na(pdcd_largo_sinp$P3154) == FALSE,
  #                                                c('keyp','keypp','P3154','P1672',var_i11)], 
  #                                   institution_allq, 
  #                                   nj_count1, 
  #                                   var_i11, 
  #                                   factor_key = TRUE)
  # 
  # dt_inst_routNPLC <-   dt_inst_routNPLC[is.na(dt_inst_routNPLC$nj_count1) == FALSE,]
  # 
  # table(duplicated(dt_inst_routNPLC$keypp), dt_inst_routNPLC$P1672)
  # table(duplicated(dt_inst_routNPLC$keypp))
  # table(dt_route_sinp$P1672)
  # 
  # # Same process for the 58 unidentifyed duplicates
  # # 
  # dt_inst_routNPLCD <- tidyr::gather(duplicated_aux_sinp[is.na(duplicated_aux_sinp$P3154) == FALSE,
  #                                                   c('keyp','keypp','P3154','P1672',var_i11)], 
  #                                   institution_allq, 
  #                                   nj_count1, 
  #                                   var_i11, 
  #                                   factor_key = TRUE)
  # 
  # dt_inst_routNPLCD <-   dt_inst_routNPLCD[is.na(dt_inst_routNPLCD$nj_count1) == FALSE,]
  
  table(duplicated(dt_inst_routNPLCD$keypp), dt_inst_routNPLCD$P1672)
  table(duplicated(duplicated_aux_sinp$keypp))
  table(dt_route_sinp_duplicated_aux$P1672)

  ### 02.1.3. Short cycle --------------------------------------------------------------------------
  
  #### 02.1.3.1 Route table -------------------------------------------------------------------------

  # Formats type_id 
  pdcd_corto$type_id <- pdcd_corto$SECUENCIA_ENCUESTA
  pdcd_corto$type_id <- sprintf("%04d", pdcd_corto$type_id)
  sum(pdcd_corto$FEX_C)
  table((pdcd_corto$type_id)%in%(dt_pj$type_id))
  
  # ___pdcd_corto_________|__pdcd_largo_p____
  #    DIRECTORIO         |   DIRECTORIO     
  #    HOG                |   SECUENCIA_P    
  #    SECUENCIA_P        |   ORDEN          
  #
  table(duplicated(pdcd_largo_sinp$keyp))
  
  pdcd_corto$keyp <- paste0(pdcd_corto$DIRECTORIO,
                            pdcd_corto$HOG,
                            pdcd_corto$SECUENCIA_P) 
  
  table(pdcd_corto$keyp %in% dt_pj$keyp)
  table(duplicated( pdcd_corto$keyp))
  
  pdcd_corto$keypp <- paste0(pdcd_corto$keyp, pdcd_corto$type_id)
  
  table(pdcd_corto$keypp %in% dt_pj$keypp)
  
  table(duplicated( pdcd_corto$keypp))
  out_aux_corto <- pdcd_corto[pdcd_corto$keypp %in%dt_pj$keypp ==  FALSE,]
  
  table(dt_pj$keyp[dt_pj$keyp %in% out_aux_corto$keyp == TRUE])
  
  pdcd_corto <- pdcd_corto[pdcd_corto$keypp %in% dt_pj$keypp ==  TRUE,]
  
  #724382.8
  dt_route_cc <- pdcd_corto[c(       'keypp',
                                     'P1693',   # route
                                     'P1695',   # last institution
                                     'P1696'
                                     , 'FEX_C'# Was the problem solved
  )]
  
  #4252.281
  dt_route_cc_out_aux <- out_aux_corto[c(    'keypp',
                                             'P1693',   # route
                                             'P1695',   # last institution
                                             'P1696'    #  Was the problem solved
                                             ,'FEX_C'
                                             ,'type_id'# type
  )]
  
  dt_ln1labs <- openxlsx::read.xlsx(sprintf(pat, "1_QSyAz-Cuoiq2TlVJuQsvQ55oz7A5FN6"))
  names(dt_route_cc_out_aux) <- c('keypp','P1672','P1674','P1685','FEX_C','type_id')
  dt_route_cc_out_aux <- merge(dt_route_cc_out_aux,dt_ln1labs, all.x = TRUE, by = 'type_id') 
  rm(dt_ln1labs)
 
  sum(dt_route_cc$FEX_C)
  sum(dt_route_cc_out_aux$FEX_C)

 dt_route_cc$FEX_C <- NULL
 
  names(dt_route_cc) <- c('keypp','P1672','P1674','P1685')

  table(dt_route_cc$P1672)
  
  dt_route_cc$cc <- 1
  dt_route_cc_out_aux$cc <- 1
  
  #### 02.1.3.2. All visited institutions list -----------------------------------------------------
  
  # var_i11 <- names(pdcd_corto)[grep('P1694S', names(pdcd_corto))] 
  # 
  # dt_inst_routCC <- tidyr::gather(pdcd_corto[is.na(pdcd_corto$type_id) == FALSE,
  #                                                   c('keyp','keypp','type_id','P1693',var_i11)], 
  #                                   institution_allq, 
  #                                   nj_count1, 
  #                                   var_i11, 
  #                                   factor_key = TRUE)
  # 
  # dt_inst_routCC <-   dt_inst_routCC[is.na(dt_inst_routCC$nj_count1) == FALSE,]
  # 
  # table(duplicated(dt_inst_routCC$keypp), dt_inst_routCC$P1693)
  # table(duplicated(dt_inst_routCC$keypp))
  # table(dt_route_cc$P1672)
  # # Same process for the 11 unidentifyed out records
  # # 
  # dt_inst_routCCD <- tidyr::gather(out_aux_corto[is.na(out_aux_corto$type_id) == FALSE,
  #                                                        c('keyp','keypp','type_id','P1693',var_i11)], 
  #                                    institution_allq, 
  #                                    nj_count1, 
  #                                    var_i11, 
  #                                    factor_key = TRUE)
  # 
  # dt_inst_routCCD <-   dt_inst_routCCD[is.na(dt_inst_routCCD$nj_count1) == FALSE,]
  # 
  # table(duplicated(dt_inst_routCCD$keypp), dt_inst_routCCD$P1693)
  # table(duplicated(dt_inst_routCCD$keypp))
  # table(dt_route_cc_out_aux$P1672)
  
  ## 02.2. Consolidated all visited institutions list ----------------------------------------------
  
  # names(dt_inst_routPLC)
  # names(dt_inst_routNPLC)
  # names(dt_inst_routCC)
  # 
  # names(dt_inst_routNPLCD)
  # names(dt_inst_routCCD)
  # 
  # names(dt_inst_routPLC)  <- c("keyp","keypp","type_id","P1672","institution_allq","nj_count1")
  # names(dt_inst_routNPLC)   <- names(dt_inst_routPLC)
  # names(dt_inst_routCC)     <- names(dt_inst_routPLC)
  # names(dt_inst_routNPLCD)  <- names(dt_inst_routPLC)
  # names(dt_inst_routCCD)    <- names(dt_inst_routPLC)
  # 
  # dt_allvisited_inst <- rbind(dt_inst_routPLC,dt_inst_routNPLC,dt_inst_routCC)
  # 
  # table(duplicated(dt_allvisited_inst$keyp))
  # table(duplicated(dt_allvisited_inst$keypp))
  # 
  # dt_allvisited_instD <- rbind(dt_inst_routNPLCD,dt_inst_routCCD)
  # 
  # table(duplicated(dt_allvisited_instD$keyp))
  # table(duplicated(dt_allvisited_instD$keypp))
  # 
  # # defines table for all visited institutions in the  cicles tables. This table
  # # observation unit is defined by individuals-problems-institutions, a singel declarant, looking to
  # # solve problem X, may have visited n institutions.
  # #
  # dt_instlabs <- openxlsx::read.xlsx(sprintf(pat, "1tB7MQUDs4TIgB7Wgoq18QlqiJ_xUaTjd"))
  # 
  # dt_allvisited_inst <- merge(dt_allvisited_inst, dt_instlabs, all.x = TRUE, by = 'institution_allq')
  # dt_allvisited_instD <- merge(dt_allvisited_instD, dt_instlabs, all.x = TRUE, by = 'institution_allq')
  # 
  # table(is.na(dt_allvisited_inst$institution_en));table(is.na(dt_allvisited_instD$institution_en))
  # 
  # openxlsx::write.xlsx(file = '2023 06 30 - all_inst.xlsx',rbind(dt_allvisited_inst,dt_allvisited_instD))
  # 
  ## 02.3. Consolidated routes table  --------------------------------------------------------------
  
  names(dt_route_p)
  names(dt_route_sinp)
  names(dt_route_cc)
  
  names(dt_route_sinp_duplicated_aux)
  names(dt_route_cc_out_aux)
  
  dt_route_p$keyp <- NULL
  dt_route_p$P3013 <- NULL
  
  dt_route_p$cc <- 0
  dt_route_sinp$cc <- 0
  dt_route_cc_out_aux$cc <- 1
  dt_route_sinp_duplicated_aux$cc <- 0
  
  dt_route_p$full_prob <- 1
  dt_route_sinp$full_prob <- 1
  dt_route_cc$full_prob <- 1
  
  dt_rt <-  plyr::rbind.fill(dt_route_p,dt_route_sinp,dt_route_cc)
  
  # Four duplicated records on the dt_rt table. At face value, this should not be an issue. Yet
  # if the records are not present in the general declaration table (so the routes differientiate 
  # them ), they should be excluded from the merging.
  # 
  # "600248110107" Declarant reported 9 problems, yet is the only one that appears in either the 
  #                priority table or the short cycle.
  # "606052141411" Declarant reported 5 problems, yet is the only one that appears in either the 
  #                priority table or the short cycle.
  # "607134111411" Declarant reported 3 problems, yet is the only one that appears in either the 
  #                priority table or the short cycle.
  # "627214121411" Declarant reported 3 problems, yet is the only one that appears in either the 
  #                priority table or the short cycle.
  #
  
  table(duplicated(dt_rt$keypp))
  table(duplicated(dt_rtD$keypp))
  
  dt_rt_aux <- dt_rt[duplicated(dt_rt$keypp),]
  
  table(dt_rt$keypp %in% dt_pj$keypp)

  dt_rt <- dt_rt[!duplicated(dt_rt$keypp),]
 
 names( dt_route_sinp_duplicated_aux)
 names( dt_route_cc_out_aux)
 names( dt_rt_aux)
  
 #partial table
 #
  
 dt_route_sinp_duplicated_aux$full_prob <- 0
 dt_route_sinp_duplicated_aux$full_prob <- 0
 
 dt_rtD <-  plyr::rbind.fill(dt_route_sinp_duplicated_aux,dt_rt_aux)
  
  table(dt_rtD$keypp %in% dt_pj$keypp)
  table(dt_rtD$keypp %in% dt_rt$keypp)
  table(dt_rtD$cc)

  # Merges complete route table with declaration table -  6258279
  # 
  
  dt_pj_rt <- merge(dt_pj,dt_rt, all.x = TRUE)
  table(is.na(dt_pj_rt$cc))
  dt_pj_rt$full_prob <- 1
  dt_pj_rt |> group_by(cc,P1685) |> summarise(fex = sum(FEX_C))
  dt_pj_rt |> group_by(P1685) |> summarise(fex = sum(FEX_C))
  dt_pj_rt |> group_by(full_prob) |> summarise(fex = sum(FEX_C))
  
  # Merges partial route table with declaration table 
  # 
  table(dt_rtD$keypp %in% dt_pj$keypp)
  dt_pj_rt1 <- merge(dt_pj,dt_rtD, all.y = TRUE)
  table(is.na(dt_pj_rt1$FEX_C))
  dt_route_cc_out_aux$full_prob <- 0
  dt_pj_rt1 <- plyr::rbind.fill(dt_pj_rt1,dt_route_cc_out_aux)
  dt_pj_rt1$full_prob <- 0
  
  dt_pj_rt1 |> group_by(cc) |> summarise(fex = sum(FEX_C))
  dt_pj_rt1 |> group_by(cc,P1685) |> summarise(fex = sum(FEX_C))
  dt_pj_rt1 |> group_by(full_prob,cc) |> summarise(fex = sum(FEX_C))
  dt_pj_rt |> group_by(full_prob,cc) |> summarise(fex = sum(FEX_C))
  
  names(dt_pj_rt)
  names(dt_pj_rt1)
  dt_pj_rt <- rbind(dt_pj_rt,dt_pj_rt1)
  dt_pj_rt$caract <- 0
  dt_pj_rt$caract[is.na(dt_pj_rt$P1685) ==  FALSE] <- 1
  
  dt_pj_rt |> group_by(full_prob) |> summarise(fex = sum(FEX_C))
  dt_pj_rt |> group_by(caract) |> summarise(fex = sum(FEX_C))
  dt_pj_rt |> group_by(cc,caract) |> summarise(fex = sum(FEX_C))
  dt_pj_rt |> group_by(cc,full_prob) |> summarise(fex = sum(FEX_C))
  
  sum(dt_pj_rt$FEX_C)

  ### There are 320 problems that where identified in the declaration stage with no records
  ### on the route section, that is about 2% of the total declaration (not counting ) the recods
  ### with route information an no data on impact. The 69 atypical records all come from the routes
  ### tables, thus they all have completely characterized problems. The main issue with them is 
  ### the fact that they do not show up in the declaration table.
  ###
  
  ## 02.4. Consolidated problem-routes-socio demographic table --------------------------------------
  
  table(dt_pj_rt$keyp %in% dt_p$keyp)
  dt_pj_rt$keyp[dt_pj_rt$keyp %in% dt_p$keyp == FALSE] <- substr(dt_pj_rt$keypp[dt_pj_rt$keyp %in% dt_p$keyp == FALSE],1,8)
  dt_pj_rt_p <- merge(dt_pj_rt, dt_p, all.x = TRUE, by = "keyp")
  
  table(dt_pj_rt_p$P220)  
  table(is.na(dt_pj_rt_p$P220),dt_pj_rt_p$DEPMUNI)
  
  # merges to general population table to get incidence and declaration
  
  dt_pj_rtu <- dt_pj_rt
  dt_pj_rtu <- dt_pj_rtu %>% group_by(keyp) %>% mutate(count = n(), p_mean = mean(impact))
  
  table(duplicated(dt_pj_rt$keyp))
  
  dt_pj_rtu <- dt_pj_rtu[!duplicated(dt_pj_rtu$keyp), c('keyp','impact','count')]
  
  table(dt_pj_rtu$keyp %in% dt_p$keyp)

  dt_p_pj <- merge( dt_p, dt_pj_rtu, all.x = TRUE, by = "keyp")
  table(is.na(dt_p_pj$count))
  dt_p_pj$pj <- dt_p_pj$count/dt_p_pj$count
  
  dt_p_pj |> group_by(pj) |> summarise(fex = sum(FEX_C))
  
  
  #openxlsx::write.xlsx(dt_pj_rt_p, 'dt_pj_rt_p.xlsx')
  
  # 03. DEFINES LEVEL 1 TRANFORMATIONAL INDICATOR --------------------------------------------------
  # (Number of people who had at least one of their justiciable problems solved) / 
  # (Number of people with at least one justiciable problem)
  # 
  # All problems fully identifyed in the declaration table
  dt1 <- dt_pj_rt_p[ dt_pj_rt_p$full_prob == 1, ]
  table(dt1$P1685);table(is.na(dt1$P1685))
  dt1 <- dt1[!is.na(dt1$P1685), ]
  table(dt1$P1685)
  dt1$P1685[dt1$P1685 == 2] <- 0
  
  table(dt1$P1672) # 3 and 4 refer to illegal/violent solution paths
  dt1$pj <- 1
  
  dt1_num <- dt1 |> group_by(keyp, FEX_C.x) |> summarise(pj = sum(P1685) )
  
  table(duplicated(dt1$keyp))
  table(dt1_num$pj)
  sum(dt1_num$FEX_C.x)
  sum(dt1$FEX_C.x[!duplicated(dt1$keyp)])

  dt1_num$pj <- dt1_num$pj/dt1_num$pj
  dt1_num$pj[is.na(dt1_num$pj == TRUE)] <- 0
  
  sum(dt1_num$FEX_C.x[dt1_num$pj == 1]) / sum(dt1_num$FEX_C.x)
  
  # All problems fully identifyed in the declaration table with no violent routes
  # 
  dt2 <- dt_pj_rt_p[ dt_pj_rt_p$full_prob == 1, ]
  table(dt2$P1685);table(is.na(dt2$P1685))
  dt2 <- dt2[!is.na(dt2$P1685), ]
  table(dt2$P1685)
  dt2$P1685[dt2$P1685 == 2] <- 0
  
  table(dt1$P1672) # 3 and 4 refer to illegal/violent solution paths
  dt2 <- dt2[ dt2$P1672 != 3, ]
  dt2 <- dt2[ dt2$P1672 != 4, ]
  
  
  dt2_num <- dt2 |> group_by(keyp, FEX_C.x) |> summarise(pj = sum(P1685) )
  
  table(duplicated(dt2$keyp))
  table(dt2_num$pj)
  table(is.na(dt2_num$pj))
  
  sum(dt2_num$FEX_C.x)
  sum(dt2$FEX_C.x[!duplicated(dt2$keyp)])
  
  dt2_num$pj <- dt2_num$pj/dt2_num$pj
  dt2_num$pj[is.na(dt2_num$pj == TRUE)] <- 0
  
  sum(dt2_num$FEX_C.x[dt2_num$pj == 1]) / sum(dt2_num$FEX_C.x)
  
  
  # Legal needs
  # 
  dt3 <- dt_pj_rt_p[ dt_pj_rt_p$full_prob == 1, ]
  table(dt3$P1685);table(is.na(dt3$P1685))
  dt3 <- dt3[!is.na(dt3$P1685), ]
  table(dt3$P1685)
  dt3$P1685[dt3$P1685 == 2] <- 0
  
  table(dt1$P1672) # 3 and 4 refer to illegal/violent solution paths
  dt3 <- dt3[ dt3$P1672 != 3, ]
  dt3 <- dt3[ dt3$P1672 != 4, ]
  dt3 <- dt3[ dt3$P1672 != 5, ]
  dt3 <- dt3[ dt3$cat_en == 'Crime', ]
                
  
  dt3_num <- dt3 |> group_by(keyp, FEX_C.x) |> summarise(pj = sum(P1685) )
  
  table(duplicated(dt3$keyp))
  table(dt3_num$pj)
  table(is.na(dt3_num$pj))
  
  sum(dt3_num$FEX_C.x)
  sum(dt3$FEX_C.x[!duplicated(dt3$keyp)])
  
  dt3_num$pj <- dt3_num$pj/dt3_num$pj
  dt3_num$pj[is.na(dt3_num$pj == TRUE)] <- 0
  
  sum(dt3_num$FEX_C.x[dt3_num$pj == 1]) / sum(dt3_num$FEX_C.x)
  
  getwd()
  saveRDS(pdcd_corto, 'dt_ciclo_corto.rds')
  saveRDS(dt_allvisited_inst, 'dt_inst.rds')
  saveRDS(dt_pj_rt_p,'dt_pj_rt_p.rds')
  saveRDS(dt_p,'dt_p.rds')
  saveRDS(dt_p_pj ,'dt_p_pj.rds')
  

  
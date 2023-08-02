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

dt_pl <- as.data.frame(dt_list[['dt_pl']])
dt_pj_rt_pl <- dt_list[['dt_pj_rt_pl']]

# 00.02 DEFINES RELEVANT GROUPING VARIABLES --------------------------------------------------------
#

## Justiciable problem count and id ----------------------------------------------------------------
## 
sum(dt_pl$FEX_C)
table(dt_pl$count); table(is.na(dt_pl$count))  #Checks total against total declaration table

dt_pl$count[is.na(dt_pl$count) == TRUE] <- 0
table(dt_pl$count)

dt_pl$jp <- dt_pl$count/dt_pl$count
table(dt_pl$jp); table(!duplicated(dt_pj_rt_pl$keyp))
dt_pl$jp[is.na(dt_pl$jp) == TRUE] <- 0

## 18 and above indicator --------------------------------------------------------------------------
## 
dt_pl$a18 <- 0
dt_pl$a18[dt_pl$P5785 > 17] <- 1
table(dt_pl$a18, dt_pl$jp)

#dt_pl <- dt_pl[dt_pl$P5785 > 14,]

## Education ---------------------------------------------------------------------------------------
##
dt_pl$edug <- 0
dt_pl$edug[dt_pl$P6210 == 'Superior o Universitaria' ] <- 1
dt_pl$edug[dt_pl$P6210 == 'Media (10-13)' ] <- 1
dt_pl$edug <- factor(dt_pl$edug)

## Victimization variable rebuild 2022/2021 ---------------------------------------------------------
## 
table(dt_pl$vic); table(is.na(dt_pl$vic))
dt_pl$vic[is.na(dt_pl$vic)] <- 0 # As the Vic variable was build under the declaration table
# all individuals who experienced a Vic circumstance  also declared JP. This is a problem.

# Household Theft - - - - - - -
# 
# 2022 
# 

tonumber <- function(x){as.numeric(dt_pl[,x])}
vicvars <- c('P541', 'P523','P525','P526','P528','P1959','P1956',#2022
             'P1392','P1960','P1179','P1343','P1315','P1286','P1976S1' #2021
             )

num <- lapply(vicvars, tonumber)
names(num) <- vicvars

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
)], na.rm = TRUE)

dt_pl$vic_2021 <- rowSums(dt_pl[c('P1392',
                                  'P1960',
                                  'P1179',
                                  'P1343',
                                  'P1315',
                                  'P1286',
                                  'P1976S1'
)], na.rm = TRUE)

table(dt_pl$vic_2022)
table(dt_pl$vic_2021)

table(is.na(dt_pl$vic_2021))
table(is.na(dt_pl$vic_2022))

dt_pl$vic_2022 <- dt_pl$vic_2022/dt_pl$vic_2022
dt_pl$vic_2021 <- dt_pl$vic_2021/dt_pl$vic_2021

dt_pl$vic_2022[is.nan(dt_pl$vic_2022)] <- 0
dt_pl$vic_2021[is.nan(dt_pl$vic_2021)] <- 0

sum(dt_pl$FEX_C[dt_pl$vic_2022 != 0])
sum(dt_pl$FEX_C[dt_pl$vic_2021 != 0])

table(dt_pl$vic_2021,dt_pl$jp)
table(dt_pl$vic_2022,dt_pl$jp) 

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



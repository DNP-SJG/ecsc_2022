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

library(ggsankey)
library(tidyverse)
library(ggalluvial)




my_theme <- theme_minimal(base_size = 20, base_family = 'Quicksand') +
  theme(
    legend.position = 'none',
    plot.title.position = 'plot',
    text = element_text(color = 'grey40'),
    plot.title = element_markdown(size = 10, margin = margin(b = 5, unit = 'mm'))
  )
theme_set(my_theme)

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



# Problem type identifiers -------------------------------------------------------------------------
dt  <-  dt_pj_rt_pl[dt_pj_rt_pl$full_prob == 1,]
dt  <-  dt_pj_rt_pl[dt_pj_rt_pl$caract == 1,]

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
'Es la forma como se resuelven los problemas aquí.',
'Tenía mucha rabia, se dejó llevar, el otro se lo merecía.')

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

c1 <- dt |> group_by(jp,njg,nje,DEPMUNI) |> summarise(fex = sum(FEX_C))

dt[dt$caract == 1 & dt$njg == 1 & dt$cc == 0,] |> group_by(P1685) |> summarise(fex = sum(FEX_C))

#openxlsx::write.xlsx(c1,'02.01 tipos de problemas.xlsx')

# 01.Sand key plot on total problem, characterization, legal needs and other -----------------------

dt <- dt_pl[dt_pl$a18 == 1, ] |> group_by(crimereporting) |> summarise(jp = mean(jp))

df <- dt |> make_long(jp,nje,njg)
dagg <- df|>dplyr::group_by(node) |> tally()

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

rm(df,dagg,pl,dt)

# 02. General route taking  ------------------------------------------------------------------------

dt  <-  dt_pj_rt_pl[dt_pj_rt_pl$caract == 1,]
c1 <- dt |> group_by(P1672, cat_labs, impact) |> summarise(fex = sum(FEX_C))

#openxlsx::write.xlsx(c1,'02.02 rutas según impacto y categoría de problema.xlsx')


# 03. Route taking on impact and problem type  -----------------------------------------------------

dt  <-  dt_pj_rt_pl[dt_pj_rt_pl$caract == 1,]
dt  <-  dt[dt$full_prob == 1,]

dt$civil <- 'Civil'
dt$civil[dt$cat_labs == 'Delitos'] <- 'Criminal'
dt$civil[dt$cat_labs == 'Conflicto armado'] <- 'Criminal'

quantile(dt$impact, c(.25, .50, .70, .90, .99))
dt$impact_a <- 'Bajo'
dt$impact_a[dt$impact < 8] <- 'Alto'

df <- dt |> group_by(civil, impact_a,P1672) |> summarise(fex = sum(FEX_C))
#openxlsx::write.xlsx(df, '02.2 civil impact route.xlsx')


df <- dt |> make_long(civil, impact_a,P1672,value = FEX_C)

dagg <- df|>dplyr::group_by(node) |> tally()

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

rm(df,dagg,pl,dt)


# 04. Route taking on sex and age  -----------------------------------------------------

dt  <-  dt_pj_rt_pl[dt_pj_rt_pl$caract == 1,]
dt  <-  dt[dt$full_prob == 1,]

dt$civil <- 'Civil'
dt$civil[dt$cat_labs == 'Delitos'] <- 'Criminal'
dt$civil[dt$cat_labs == 'Conflicto armado'] <- 'Criminal'

quantile(dt$impact, c(.25, .50, .70, .90, .99))
dt$impact_a <- 'Bajo'
dt$impact_a[dt$impact < 8] <- 'Alto'


c1 <- dt |> group_by(P1672, cat_labs, civil,impact_a,impact,P220,P5785) |> summarise(fex = sum(FEX_Cy))
openxlsx::write.xlsx(c1, '02.2 route taking and demographics.xlsx')


# 05. Route taking on sex, education SWB  -----------------------------------------------------

dt  <-  dt_pj_rt_pl[dt_pj_rt_pl$caract == 1,]
dt  <-  dt[dt$full_prob == 1,]

dt$civil <- 'Civil'
dt$civil[dt$cat_labs == 'Delitos'] <- 'Criminal'
dt$civil[dt$cat_labs == 'Conflicto armado'] <- 'Criminal'

quantile(dt$impact, c(.25, .50, .70, .90, .99))
dt$impact_a <- 'Bajo'
dt$impact_a[dt$impact < 8] <- 'Alto'


c1 <- dt |> group_by(P1672, cat_labs, civil,impact_a,impact,P220,P5785) |> summarise(fex = sum(FEX_Cy))
#openxlsx::write.xlsx(c1, '02.2 route taking and demographics.xlsx')


c1 <- dt |> group_by(P1672, cat_labs, civil,impact_a,impact,
                     P220,P5785,edug,P3303, swbi,P3503S1_1,pea,single) |> summarise(fex = sum(FEX_Cy))

openxlsx::write.xlsx(c1, '02.3 route taking and demographics II.xlsx')

##  education by route
dt1 <- dt
dt1$P1672 <- as.character(dt1$P1672)
dt1$P1672[dt1$P1672 == "Acudió a una institución, autoridad o persona particular"] <- "Instución o tercero"
dt1$P1672[dt1$P1672 == "Intentó llegar a un acuerdo directamente con quien tuvo el problema"] <- "Acuerdo directo"
dt1$P1672[dt1$P1672 == "Actuó de forma violenta "] <- "Actor ilegal o acción violenta"
dt1$P1672[dt1$P1672 == "Acudió a un actor ilegal "] <- "Actor ilegal o acción violenta"
dt1$P1672[dt1$P1672 == "No hizo nada"] <- "Inacción"

dt1 <- dt1[dt1$P1672 != "Actor ilegal o acción violenta", ]
dt1$civil <- 'Civil'
dt1$civil[dt1$cat_labs == 'Delitos'] <- 'Criminal'
dt1$civil[dt1$cat_labs == 'Conflicto armado'] <- 'Criminal'

dt1$P1672 <- paste0(dt1$civil,"-", dt1$P1672)

dt1 <- dt1 |> group_by(edug,P1672) |> summarise(fex = sum(FEX_Cx))
dt1 <- dt1 |> group_by(P1672) |> mutate(proc = (fex/sum(fex) * 100))

my_palette  <-  c("#00AFBB", "#E7B800")

segment_helper <- dt1 |> select(edug, P1672, proc) |>
  pivot_wider(names_from = edug, values_from = proc, names_prefix = 'educ_') |>
  mutate( change = educ_1 - educ_0,
          P1672  = fct_reorder(P1672, educ_1 * if_else(change < 0, -1, 1))
  )

dt1 |>
  ggplot(aes(x = proc, y = P1672, col = as.factor(edug))) +
  geom_segment(
    data = segment_helper,
    aes(x = educ_0, xend = educ_1, y = P1672, yend = P1672),
    col = 'grey60',
    size = 1.25) +
  geom_point(size = 6) +
  labs(
    x = 'Participación',
    y =  'Ruta de acción') +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()) +
  scale_color_manual(values=c( "#E7B800", "#00AFBB"))

##  pea by route
dt1 <- dt
dt1$P1672 <- as.character(dt1$P1672)
dt1$P1672[dt1$P1672 == "Acudió a una institución, autoridad o persona particular"] <- "Instución o tercero"
dt1$P1672[dt1$P1672 == "Intentó llegar a un acuerdo directamente con quien tuvo el problema"] <- "Acuerdo directo"
dt1$P1672[dt1$P1672 == "Actuó de forma violenta "] <- "Actor ilegal o acción violenta"
dt1$P1672[dt1$P1672 == "Acudió a un actor ilegal "] <- "Actor ilegal o acción violenta"
dt1$P1672[dt1$P1672 == "No hizo nada"] <- "Inacción"

dt1 <- dt1[dt1$P1672 != "Actor ilegal o acción violenta", ]
dt1$civil <- 'Civil'
dt1$civil[dt1$cat_labs == 'Delitos'] <- 'Criminal'
dt1$civil[dt1$cat_labs == 'Conflicto armado'] <- 'Criminal'

dt1$P1672 <- paste0(dt1$civil,"-", dt1$P1672)

dt1 <- dt1 |> group_by(pea,P1672) |> summarise(fex = sum(FEX_Cx))
dt1 <- dt1 |> group_by(P1672) |> mutate(proc = (fex/sum(fex) * 100))

my_palette  <-  c("#00AFBB", "#E7B800")

segment_helper <- dt1 |> select(pea, P1672, proc) |>
  pivot_wider(names_from = pea, values_from = proc, names_prefix = 'pea_') |>
  mutate( change = pea_1 - pea_0,
          P1672  = fct_reorder(P1672, pea_1 * if_else(change < 0, -1, 1))
  )

dt1 |>
  ggplot(aes(x = proc, y = P1672, col = as.factor(pea))) +
  geom_segment(
    data = segment_helper,
    aes(x = pea_0, xend = pea_1, y = P1672, yend = P1672),
    col = 'grey60',
    size = 1.25) +
  geom_point(size = 6) +
  labs(
    x = 'Participación',
    y =  'Ruta de acción') +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()) +
  scale_color_manual(values=c( "#E7B800", "#00AFBB"))

##  conditional dumbell
##  
dt1 <- dt[dt$edug == 0,]


dt1$P1672 <- as.character(dt1$P1672)
dt1$P1672[dt1$P1672 == "Acudió a una institución, autoridad o persona particular"] <- "Instución o tercero"
dt1$P1672[dt1$P1672 == "Intentó llegar a un acuerdo directamente con quien tuvo el problema"] <- "Acuerdo directo"
dt1$P1672[dt1$P1672 == "Actuó de forma violenta "] <- "Actor ilegal o acción violenta"
dt1$P1672[dt1$P1672 == "Acudió a un actor ilegal "] <- "Actor ilegal o acción violenta"
dt1$P1672[dt1$P1672 == "No hizo nada"] <- "Inacción"

dt1$inac <- 'Acción'
dt1$inac[dt1$P1672 == "Inacción"] <- "Inacción"

dt1 <- dt1[dt1$P1672 != "Actor ilegal o acción violenta", ]

dt1$civil <- 'Civil'
dt1$civil[dt1$cat_labs == 'Delitos'] <- 'Criminal'
dt1$civil[dt1$cat_labs == 'Conflicto armado'] <- 'Criminal'


dt1$cat_labs[dt1$cat_labs == 'Conflicto armado'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Discriminación'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Educación'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Medio ambiente'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Propiedad'] <- 'Otro' 

dt1 <- dt1 |> group_by(inac,cat_labs) |> summarise(fex = sum(FEX_Cx))
dt1 <- dt1 |> group_by(cat_labs) |> mutate(proc = (fex/sum(fex) * 100))

dt1$cat_labs <- as.factor(dt1$cat_labs)

my_palette  <-  c("#00AFBB", "#E7B800")

segment_helper <- dt1 |> select(inac, cat_labs, proc) |>
  pivot_wider(names_from = inac, values_from = proc, names_prefix = 'inac_') |>
  mutate( change = inac_Acción - inac_Inacción, cat_labs  = fct_reorder(cat_labs, change) )

dt1 <- merge(dt1,segment_helper)

dt1 |>
  ggplot(aes(x = proc, y = fct_reorder(cat_labs, change), change, col = as.factor(inac))) +
  geom_segment(
    data = segment_helper,
    aes(x = inac_Inacción, xend = inac_Acción, y = fct_reorder(cat_labs, change), yend = fct_reorder(cat_labs, change)),
    col = 'grey60',
    size = 1.25) +
  geom_point(size = 6) +
  labs(
    x = 'Participación',
    y =  'Ruta de acción') +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()) +
  scale_color_manual(values=c( "#E7B800", "#00AFBB"))

# tile plot
#
#

dt1 <- dt


dt1$P1672 <- as.character(dt1$P1672)
dt1$P1672[dt1$P1672 == "Acudió a una institución, autoridad o persona particular"] <- "Instución o tercero"
dt1$P1672[dt1$P1672 == "Intentó llegar a un acuerdo directamente con quien tuvo el problema"] <- "Acuerdo directo"
dt1$P1672[dt1$P1672 == "Actuó de forma violenta "] <- "Actor ilegal o acción violenta"
dt1$P1672[dt1$P1672 == "Acudió a un actor ilegal "] <- "Actor ilegal o acción violenta"
dt1$P1672[dt1$P1672 == "No hizo nada"] <- "Inacción"

dt1$inac <- 'Acción'
dt1$inac[dt1$P1672 == "Inacción"] <- "Inacción"

dt1 <- dt1[dt1$P1672 != "Actor ilegal o acción violenta", ]

dt1$civil <- 'Civil'
dt1$civil[dt1$cat_labs == 'Delitos'] <- 'Criminal'
dt1$civil[dt1$cat_labs == 'Conflicto armado'] <- 'Criminal'


dt1$cat_labs[dt1$cat_labs == 'Conflicto armado'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Discriminación'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Educación'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Medio ambiente'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Propiedad'] <- 'Otro' 

dt1$pcat <- paste0(dt1$cat_labs,'-',dt1$type_labs)

a <- dt1[dt1$civil == 'Civil',] |> group_by(pcat,P1672) |> summarise(fex = sum(FEX_C))
#openxlsx::write.xlsx(a, '01.13.problem number by type.xlsx')

b <- a |> group_by(P1672) |> arrange(desc(fex)) |>  mutate(id = row_number())
b$pcat[b$id > 4] <- 'Otro' 
b <- b |> group_by(P1672,pcat) |> summarise(fex = sum(fex))
# openxlsx::write.xlsx(b, '02.03. top problem type by route.xlsx')


# impact and inaction by SWB groups.

dt1 <- dt


dt1$P1672 <- as.character(dt1$P1672)
dt1$P1672[dt1$P1672 == "Acudió a una institución, autoridad o persona particular"] <- "Instución o tercero"
dt1$P1672[dt1$P1672 == "Intentó llegar a un acuerdo directamente con quien tuvo el problema"] <- "Acuerdo directo"
dt1$P1672[dt1$P1672 == "Actuó de forma violenta "] <- "Actor ilegal o acción violenta"
dt1$P1672[dt1$P1672 == "Acudió a un actor ilegal "] <- "Actor ilegal o acción violenta"
dt1$P1672[dt1$P1672 == "No hizo nada"] <- "Inacción"

dt1$inac <- 0
dt1$inac[dt1$P1672 == "Inacción"] <- 1

dt1 <- dt1[dt1$P1672 != "Actor ilegal o acción violenta", ]

dt1$civil <- 'Civil'
dt1$civil[dt1$cat_labs == 'Delitos'] <- 'Criminal'
dt1$civil[dt1$cat_labs == 'Conflicto armado'] <- 'Criminal'


dt1$cat_labs[dt1$cat_labs == 'Conflicto armado'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Discriminación'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Educación'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Medio ambiente'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Propiedad'] <- 'Otro' 

dt1 <- dt1[dt1$cat_labs != 'Otro',]
dt1 <- dt1[is.na(dt1$impact) != TRUE,]
dt1 <- dt1[is.na(dt1$swbi) != TRUE,]

table(is.na(dt1$cat_labs))


a <- dt1[dt1$swbi == 1,] |> group_by(cat_labs) |> summarise(imp = mean(impact,na.rm = TRUE))
b <- dt1[dt1$swbi == 1,] |> group_by(cat_labs) |> summarise(ina = mean(inac))
c <- merge(a,b, all.x = TRUE)
c$swbi <- 'Bajo' 

a <- dt1[dt1$swbi == 0,] |> group_by(cat_labs) |> summarise(imp = mean(impact,na.rm = TRUE))
b <- dt1[dt1$swbi == 0,] |> group_by(cat_labs) |> summarise(ina = mean(inac))
d <- merge(a,b, all.x = TRUE)
d$swbi <- 'Alto' 

e <- rbind(c,d)

# openxlsx::write.xlsx(e, '2.4.SWB impact and inaction rate scatters by tipology.xlsx')

# 06. Route taking by household characteristics  ---------------------------------------------------
dt  <-  dt_pj_rt_pl[dt_pj_rt_pl$caract == 1,]
dt1  <-  dt

dt1$P1672 <- as.character(dt1$P1672)
dt1$P1672[dt1$P1672 == "Acudió a una institución, autoridad o persona particular"] <- "Instución o tercero"
dt1$P1672[dt1$P1672 == "Intentó llegar a un acuerdo directamente con quien tuvo el problema"] <- "Acuerdo directo"
dt1$P1672[dt1$P1672 == "Actuó de forma violenta "] <- "Actor ilegal o acción violenta"
dt1$P1672[dt1$P1672 == "Acudió a un actor ilegal "] <- "Actor ilegal o acción violenta"
dt1$P1672[dt1$P1672 == "No hizo nada"] <- "Inacción"

dt1$inac <- 0
dt1$inac[dt1$P1672 == "Inacción"] <- 1

dt1 <- dt1[dt1$P1672 != "Actor ilegal o acción violenta", ]

dt1$civil <- 'Civil'
dt1$civil[dt1$cat_labs == 'Delitos'] <- 'Criminal'
dt1$civil[dt1$cat_labs == 'Conflicto armado'] <- 'Criminal'

dt1$cat_labs[dt1$cat_labs == 'Conflicto armado'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Discriminación'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Educación'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Medio ambiente'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Propiedad'] <- 'Otro' 


dt1$impact_a <- 'Bajo'
dt1$impact_a[dt1$impact < 8] <- 'Alto'

dt2 <- dt1 |> group_by(type_labs,cat_labs, Clase, impact, impact_a, inac) |> summarise(fex = mean(FEX_Cx))

#openxlsx::write.xlsx(dt1, '02.05.route and hh charactersiticas.xlsx')

## Perception and routes ---------------------------------------------------------------------------
## 
dt  <-  dt_pj_rt_pl[dt_pj_rt_pl$caract == 1,]
dt1  <-  dt

dt1$P1672 <- as.character(dt1$P1672)
dt1$P1672[dt1$P1672 == "Acudió a una institución, autoridad o persona particular"] <- "Instución o tercero"
dt1$P1672[dt1$P1672 == "Intentó llegar a un acuerdo directamente con quien tuvo el problema"] <- "Acuerdo directo"
dt1$P1672[dt1$P1672 == "Actuó de forma violenta "] <- "Actor ilegal o acción violenta"
dt1$P1672[dt1$P1672 == "Acudió a un actor ilegal "] <- "Actor ilegal o acción violenta"
dt1$P1672[dt1$P1672 == "No hizo nada"] <- "Inacción"

dt1$inac <- 0
dt1$inac[dt1$P1672 == "Inacción"] <- 1

dt1 <- dt1[dt1$P1672 != "Actor ilegal o acción violenta", ]

dt1$civil <- 'Civil'
dt1$civil[dt1$cat_labs == 'Delitos'] <- 'Criminal'
dt1$civil[dt1$cat_labs == 'Conflicto armado'] <- 'Criminal'

dt1$cat_labs[dt1$cat_labs == 'Conflicto armado'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Discriminación'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Educación'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Medio ambiente'] <- 'Otro' 
dt1$cat_labs[dt1$cat_labs == 'Propiedad'] <- 'Otro' 


dt1$impact_a <- 'Bajo'
dt1$impact_a[dt1$impact < 8] <- 'Alto'

dt2 <- dt1 |> group_by(P1182S1,P1181S1,P1181S2, 
                       cat_labs, safe_local,safe_WaN,
                       safe_city,Clase, impact, inac,
                       P1672,P564, civil
                       ) |> summarise(fex = mean(FEX_Cx))

openxlsx::write.xlsx(dt2, '02.06.route and security perception II.xlsx')

# ------------------------------------------------------------------------------------------------ #
# OUT TABLES IMPACT - ECSC 2022
#  2023/08/26
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

# impact distribution breaks - - - - - -

tab1 <- dt |> group_by(impact,P220,old,edug,pea,dis,ownedh,cat_labs ) |> summarise(fex = sum(FEX_Cx, na.rm = TRUE))
#openxlsx::write.xlsx(tab1, 'impact groupings by demographics.xlsx')

library(ggplot2)
library(ggridges)
theme_set(theme_minimal())

fun_color_range <- colorRampPalette(c("#2FB0B2", "#E7B800"))
my_colors <- fun_color_range(11)
my_colors <- fun_color_range(3)
sc <- scale_color_gradientn(colors = my_colors) 

ggplot(
  dt, 
  aes(x = impact, y = cat_labs, fill = stat(x),group = cat_labs, alpha = .7)) +
  
  geom_density_ridges_gradient(scale = 5, 
                               size = 0.3, 
                               rel_min_height = 0.02,
                               alpha = 0.8) +
  
  scale_fill_gradient(low="#2FB0B2", 
                      high="#E7B800", 
                      limits = c(0, 10), 
                      oob = scales::squish,
                      name = " Impacto") +
  
  theme(legend.position = "top") +
  labs(title = 'Impacto declarado del problema') 


dt$civil <- 1
dt$civil[dt$cat_labs == 'Delitos'] <- 0
dt$civil[dt$cat_labs == 'Conflicto armado'] <- 0

table( dt$cat_labs,dt$civil)


  wilcox.test( impact ~ civil, data = dt)
  t.test( impact ~ civil, data = dt)
  
# dumbbell  plot over impact and crime/civil issues
# 
# 
# 

  dt$age <- '18-21'
  dt$age[ dt$P5785 > 21 & dt$P5785 < 33 ] <- '22-32'
  dt$age[ dt$P5785 > 32 & dt$P5785 < 44 ] <- '33-43'
  dt$age[ dt$P5785 > 43 & dt$P5785 < 55 ] <- '44-54'
  dt$age[ dt$P5785 > 54 & dt$P5785 < 66 ] <- '55-65'
  dt$age[ dt$P5785 > 65 & dt$P5785 < 76 ] <- '66-76'
  dt$age[ dt$P5785 > 75  ] <- '76 - 109'

table(dt$P5785,dt$age)

  
  font_add_google('Quicksand', 'Quicksand')
  showtext_auto()
  showtext_opts(dpi = 300)
  
  
  my_theme <- theme_minimal(base_size = 20, base_family = 'Quicksand') +
    theme(
      legend.position = 'none',
      plot.title.position = 'plot',
      text = element_text(color = 'grey40'),
      plot.title = element_markdown(size = 10, margin = margin(b = 5, unit = 'mm'))
    )
  theme_set(my_theme)

  # age and problem type
  #
  dt1 <- dt |> group_by(age,civil) |> summarise(impact = mean (impact))

  my_palette  <-  c("#00AFBB", "#E7B800")
  segment_helper <- dt1 |>
    select(age, civil, impact) |>
    pivot_wider(names_from = civil, values_from = impact, names_prefix = 'problem_') |>
    mutate(
      change = problem_1 -problem_0,
      country = fct_reorder(age, problem_1 * if_else(change < 0, -1, 1))
    )
  
dt1 |>
    ggplot(aes(x = impact, y = age, col = as.factor(civil))) +
    geom_segment(
      data = segment_helper,
      aes(x = problem_0, xend = problem_1, y = age, yend = age),
      col = 'grey60',
      size = 1.25) +
    geom_point(size = 6) +
    labs(
      x = 'Impacto medio',
      y =  'Grupo de edad') +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank()) +
    scale_color_manual(values=c( "#E7B800", "#00AFBB"))

# sex and problem cat
#
dt2 <- dt |> group_by(P220,cat_labs) |> summarise(impact = mean (impact))



segment_helper <- dt2 |>
  select(cat_labs, P220, impact) |>
  pivot_wider(names_from = P220, values_from = impact, names_prefix = 'sex_') |>
  mutate(
    change = sex_Mujer - sex_Hombre,
    cat_labs = fct_reorder(cat_labs, sex_Mujer * if_else(change < 0, -1, 1))
  )

dt2 |>
  ggplot(aes(x = impact, y = cat_labs, col = as.factor(P220))) +
  geom_segment(
    data = segment_helper,
    aes(x = sex_Hombre, xend = sex_Mujer, y = cat_labs, yend = cat_labs),
    col = 'grey60',
    size = 1.25) +
  geom_point(size = 6) +
  labs(
    x = 'Impacto medio',
    y =  'Grupo de problemas') +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()) +
  scale_color_manual(values = c( "#E7B800", "#00AFBB"))

# ------------------------------------------------------------------------------------------------ #
# OUT TABLES PROBLEM CLUSTERS AND NETWORK - ECSC 2022		
#  2023/08/30		
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

# Clusters --
library(ggtext)
library(showtext)
library(factoextra)
library(cluster)
library(purrr)
library(pheatmap)

# Networks --
library(visNetwork)
library(geomnet)
library(igraph)
library(circlize)
library(ggraph)
library(qgraph)

# Theme --
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

# 01. CLUSTER ANALYSIS -----------------------------------------------------------------------------

# Subsets for fully characterized problems, complete records con SWB, problems with more than
# 900 respondents and individuals who declared only one experienced problem -- -- -- -- -- -- -- --

## full problems		
dt <- dt_pj_rt_pl[dt_pj_rt_pl$full_prob == 1,]

## ful SWB records		
dt <- dt[is.na(dt$swbi) == FALSE,]

## Types with more than 900 declared issues
types <- c(
  'Consumo',
  'Delitos' ,
  'Deudas' ,
  'Entorno',
  'Familiares',
  'Salud y pensión',
  'Servicios públicos',
  'Vivenda'
)		
dt <- dt[dt$cat_labs %in% types == TRUE,]

## Respondents with less than 2 issues
## 
dt <- dt[dt$nj_count1 < 2,]

# Relabels categorical vaiables to get mean groupings -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -

## sex
dt$P220 <- as.character(dt$P220)

dt$P220[dt$P220 == 'Hombre'] <- 1
dt$P220[dt$P220 == 'Mujer'] <- 0

dt$P220 <- as.numeric(dt$P220)

## education		
dt$edug <- as.numeric(dt$edug)
dt$edug[dt$edug== 1 ] <- 0
dt$edug[dt$edug == 2] <- 1

## internet access		
dt$P3303 <- as.numeric(dt$P3303 )
dt$P3303 [dt$P3303 == 1 ] <- 1
dt$P3303 [dt$P3303  == 2] <- 0

## impact

# Creates impact category based on impact level
#
dt$impact_c <- 'bajo'		
dt$impact_c[dt$impact > 6] <- 'alto'		
dt$c1 <- paste0(dt$cat_labs, ' ', dt$impact_c)

# Collapses at the problem category level  -- -- -- -- -- -- -- - -- -- -- -- -- -- - -- -- -- -- --
#
dt1 <- dt |> group_by(c1) |> summarise(		
                                       sex     = mean (P220),		
                                       dis     = mean (dis),
                                       edug    = mean(edug),
                                       pea     = mean(pea),
                                       int     = mean(P3303),
                                      swbi     = mean(swbi),
                                      safe_WaN = mean (safe_WaN),
                                      ownedh   = mean (ownedh),
                                      old      = mean (old)
)

dt1 <- as.data.frame(dt1)
rownames(dt1) <- dt1$c1
dt1$c1 <- NULL

# Normalization of records
#		
dt.scaled <- scale(dt1)		
dist.eucl <- dist(dt.scaled, method = "euclidean")

# Distance matrix		
#		
round(as.matrix(dist.eucl), 1)

# Distance correlation matrix
#		
dist.cor <- get_dist(dt.scaled, method = "pearson")
fviz_dist(dist.eucl,		
          gradient = list(low = "#2B3045", mid = "#48B1CB", high = "#FFC411"))

# K-means clustering -- -- -- -- -- -- -- - -- -- -- -- -- -- - -- -- -- -- -- -- -- -- -- -- -- --
#
set.seed(123) # for reproducibility

fviz_nbclust(dt.scaled, kmeans, method = "gap_stat")

fun_color_range <- colorRampPalette(c("#48B1CB", "#E7B800"))
my_colors <- fun_color_range(4)
sc <- scale_color_gradientn(colors = my_colors)

km.res <- kmeans(dt.scaled, 4, nstart = 100)
fviz_cluster(km.res, data = dt.scaled, palette = my_colors,stand = TRUE,
             ggtheme = my_theme,labelsize = 26,repel = 1,star.plot = TRUE)

print(km.res)		

c_means <- aggregate(dt1, by=list(cluster=km.res$cluster), mean)

# Hierarchical cluster -- -- -- -- -- -- -- - -- -- -- -- -- -- - -- -- -- -- -- -- -- -- -- -- -- --
#
hc.cut <- hcut(dt.scaled, k = 4, hc_method = "complete")		

# Visualize dendrogram		
fviz_dend(hc.cut, show_labels = TRUE, rect = TRUE)		

# Visualize cluster		
#		

fviz_cluster(hc.cut, ellipse.type = "convex")		
gap_statres.hc <- hclust(dist(dt.scaled),  method = "ward.D2")		

pheatmap(t(dt.scaled), cutree_cols = 6)

# 02. NETWORKS -------------------------------------------------------------------------------------

# Circular network -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
# 

dt  <-  dt_pj_rt_pl[dt_pj_rt_pl$full_prob == 1,]
dt[!duplicated(dt$keyp),] |> group_by(nj_count1) |> summarise(fex = sum(FEX_C))
dt1 <-  dt[dt$nj_count1 > 1,]
dt1 <- dt1 |> group_by(keyp) |>  mutate(id = row_number())
dt1 <- dt1[c('keyp','cat_labs','type_labs','FEX_C','id','impact','nj_count1')]
dtm  <-  dt1


table(dt1$id)
for (i in 1:max(dt1$id)){
     aux    <-   dt1[dt1$id == i,c('cat_labs','keyp') ]
     names(aux)    <-  c(paste0('cat_labs',i),'keyp')
     dtm      <-  merge(dtm,aux, all.x = T, by = 'keyp' )
}

list_dtm <- list()
for (i in 1:max(dt1$id)){
     a  <-   dtm[,c('keyp','nj_count1','FEX_C','cat_labs',paste0('cat_labs',i))]
     names(a) <- c('keyp','nj_count1','FEX_C','in','out')
     list_dtm[[i]] <- a
     names(list_dtm[i]) <- paste0('a',i)
     rm(a)
}

list_dtm[[1]]
dta <- do.call(rbind, list_dtm)

table(is.na(dta$out))
dta <- dta[is.na(dta$out) == FALSE,]
cats <- dt[!duplicated(dt$cat_labs), c('cat_labs','cat_id')]
cats$cat_idin <- cats$cat_id
cats$cat_idout <- cats$cat_id

# dta <- merge(dta,cats[c('cat_labs','cat_idin')], all.x = TRUE, by.x  = 'in', by.y = 'cat_labs')
# dta <- merge(dta,cats[c('cat_labs','cat_idout')], all.x = TRUE, by.x  = 'out', by.y = 'cat_labs')

dta$equal <- dta$`in` == dta$out
dta$keyppp <- paste0(dta$keyp,dta$cat_idout,dta$cat_outin)

dtb <- dta |> group_by(`in`,out) |> summarise(fex = sum(FEX_C))
names(dtb) <- c('to','from','width')

circos.clear()
circos.par(start.degree =90,
           gap.degree = 3,
           track.margin = c(-0.1, 0.1),
           points.overflow.warning = FALSE)

par(mar = rep(0, 4))

set.seed(123)
fun_color_range <- colorRampPalette(c("#48B1CB", "#E7B800"))
my_colors <- fun_color_range(15)
sc <- scale_color_gradientn(colors = my_colors)

chordDiagram(
  x = dtb,
  transparency = 0.25,
  directional = 1,
  grid.col = my_colors,
  direction.type = c("arrows", "diffHeight"),
  diffHeight  = -0.04,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow",
  link.sort = FALSE,
  link.largest.ontop = TRUE)
circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    circos.text(
      x = mean(xlim),
      y = 3.2,
      labels = sector.index,
      facing = "bending",
      cex = .6
    )
  }
)

# Communities network -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- - 
# 
table(dt$P220)
dt  <-  dt_pj_rt_pl[dt_pj_rt_pl$full_prob == 1,]
dt  <-  dt[dt$P3303 == 'No',]
dt$impact_c <- 'bajo'		
dt$impact_c[dt$impact > 6] <- 'alto'		
dt$c1 <- paste0(dt$cat_labs, ' ', dt$impact_c)

dt1 <-  dt[dt$nj_count1 > 1,]

dt1 <- dt1 |> group_by(keyp) |>  mutate(id = row_number())
dt1 <- dt1[c('keyp','cat_labs','type_labs','FEX_C','id','impact','nj_count1')]
dtm  <-  dt1


table(dt1$id)
for (i in 1:max(dt1$id)){
  aux    <-   dt1[dt1$id == i,c('cat_labs','keyp') ]
  names(aux)    <-  c(paste0('cat_labs',i),'keyp')
  dtm      <-  merge(dtm,aux, all.x = T, by = 'keyp' )
}

list_dtm <- list()
for (i in 1:max(dt1$id)){
  a  <-   dtm[,c('keyp','nj_count1','FEX_C','cat_labs',paste0('cat_labs',i))]
  names(a) <- c('keyp','nj_count1','FEX_C','in','out')
  list_dtm[[i]] <- a
  names(list_dtm[i]) <- paste0('a',i)
  rm(a)
}

list_dtm[[1]]
dta <- do.call(rbind, list_dtm)

table(is.na(dta$out))
dta <- dta[is.na(dta$out) == FALSE,]
cats <- dt[!duplicated(dt$cat_labs), c('cat_labs','cat_id')]
cats$cat_idin <- cats$cat_id
cats$cat_idout <- cats$cat_id

# dta <- merge(dta,cats[c('cat_labs','cat_idin')], all.x = TRUE, by.x  = 'in', by.y = 'cat_labs')
# dta <- merge(dta,cats[c('cat_labs','cat_idout')], all.x = TRUE, by.x  = 'out', by.y = 'cat_labs')

dta$equal <- dta$`in` == dta$out
dta$keyppp <- paste0(dta$keyp,dta$cat_idout,dta$cat_outin)

dtb <- dta |> group_by(`in`,out) |> summarise(fex = sum(FEX_C))
names(dtb) <- c('to','from','width')

dtb <- dtb[dtb$to != dtb$from,]
dtb <- dtb[dtb$to != 'Delitos',]
dtb <- dtb[dtb$from != 'Delitos',]

dtb <- dtb |> group_by(to) |> arrange(desc(width)) |>  mutate(id = row_number())
dtb <- dtb[!duplicated(dtb$width) == FALSE,]

dtb <- dtb[dtb$id < 6,]

graph <- igraph::graph_from_data_frame(dtb, directed = F)

sgc <- igraph::cluster_edge_betweenness(graph)
sgc$membership


set.seed(123)

# comminity
lou <- cluster_louvain(graph)
lyt <- layout_with_fr(graph)
V(graph)$membership <- lou$membership

# Layout
e <- get.edgelist(graph,names=FALSE)
l <- qgraph.layout.fruchtermanreingold(e,vcount = vcount(graph))
l <- qgraph.layout.fruchtermanreingold(e,vcount = vcount(graph),
                                       area = 10*(vcount(graph)^2),
                                       repulse.rad = (vcount(graph)^3.1))

# vertex -- -- 

deg <- igraph::degree(graph)
V(graph)$vertx.size <- graph
V(graph)$color <- my_colors[V(graph)]



# edges -- -- 
edge.start <- ends(graph, es = E(graph), names=F)[,1]
edge.col <- V(graph)$color[edge.start]
E(graph)$weight <- E(graph)$width/ 20000

windowsFonts("Helvetica" = windowsFont("Helvetica"))

plot(lou,
     graph, 
     layout = lyt,
     
     vertex.size = deg * 4,
     vertex.color = adjustcolor(V(graph)$color, alpha.f = .6) ,
     #vertex.label.font = 8,
     vertex.label.family = 'Helvetica',
     vertex.label.color = "gray20",
     vertex.label.cex = 1.1 ,
     vertex.label.degree = -pi/2,
     vertex.frame.width=0,
     vertex.label.dist = 0.4,
     
     edge.width =  E(graph)$weight ,
     edge.arrow.size = 0.5,
     edge.color = adjustcolor(edge.col, alpha.f = .6) ,
     edge.curved = 0,
     )



graph <- igraph::graph_from_data_frame(dtb, directed = F)

clp <- cluster_louvain(graph)
V(graph)$color <- clp$membership

a1 <- as.data.frame(get.edgelist(graph))
E(graph)$color <- map2_dbl(a1$V1, a1$V2, ~ {
  ifelse(
    V(graph)$color[V(graph)[.x]] ==
      V(graph)$color[V(graph)[.y]],
    V(graph)$color[V(graph)[.x]],
    9999) 
})

ggraph::ggraph(graph, layout='fr') + 
  geom_edge_link0(aes(filter=color!=9999 ,color=as.factor(color)), width=0.6, alpha=0.35) + 
  geom_edge_link0(aes(filter=color==9999), color='grey', width=0.5, alpha=0.25) + 
  geom_node_point(aes(color=as.factor(color)), size=3, alpha=0.75) + 
  theme_graph(base_family = 'Helvetica')


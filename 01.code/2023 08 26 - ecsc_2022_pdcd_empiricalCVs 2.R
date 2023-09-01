Sys.setlocale(locale="es_ES.UTF-8")

dt_fil  <- '1y9ATpg7NrwqitwMEm5BbXvnLWlb8Xyth'
dt_pat  <- 'https://docs.google.com/uc?id=%s&export=download'
dt_tab  <- data.frame(openxlsx::read.xlsx(sprintf(dt_pat, dt_fil,download.method = 'curl')))

dt_pro <- openxlsx::read.xlsx('L:/01.ecsc2020/01.tablas/lab_tipologia.xlsx')
dt_tab <- merge(dt_tab,dt_pro[c('P3013','P3013_cat_labenS')], all.x = T, by = 'P3013')
table(is.na(dt_tab$P3013_cat_labenS))
table((dt_tab$P3013_cat_labenS))

delitos <- openxlsx::read.xlsx('C:/Users/luisc/Desktop/New folder/tipolog_delitos.xlsx')
table(dt_tab$P3013 %in% delitos$P3013)
table(delitos$P3013 %in% dt_tab$P3013)
dt_tab <- merge(dt_tab,delitos, all.x = T, by = 'P3013')
dt_tab$delitos[is.na(dt_tab$delitos) == T] <- 0

table(dt_tab$delitos)
# a. define marcos --------------------------------------------------------------------------------
# * a.1 necesidades jurídicas ---------------------------------------------------------------------
dt_ln <- dt_tab[dt_tab$P1685 != 9,] # reporte de soución caracterizado
#dt_ln <- dt_ln[dt_ln$P1672 != 2,]   # excluye ruta de acuerdo directo
dt_ln <- dt_ln[is.na(dt_ln$P1687) != T,] 
dt_ln <- dt_ln[dt_ln$DEPMUNI != 0,] # reporte de soución caracterizado


dt_ln$P1685[dt_ln$P1685 == 2] <- 0
#sum(dt_ln$FEX_C)

# * a.2 problemas justiciables ---------------------------------------------------------------------
dt_lp <- dt_ln 

#sum(dt_lp$FEX_C)

library(dplyr)

dt_lp %>% group_by(P3013_cat_labenS) %>% summarise( fex = sum(FEX_C))

# b. boostrap para ECV -----------------------------------------------------------------------------
# * b.1 necesidades jurídicas grupo nacional + cat de problemas ------------------------------------

# estratos sobre los que se desarrollará el muestreo

dt_lp <- dt_pj_rt_pl[dt_pj_rt_pl$full_prob == 1,]
stra <- unique(dt_pj_rt_pl)

# muestrea aleatoria del 40% (adhoc) del total de unidades en cada grupo de problema
# construcción de media y desviación en un vector de salida para cada agrupación
# imputación de cada grupo estimado junto con las estimacipnes puntuales de los fexc

boon  <- 1000
dtout <- data.frame(problem = NA, hatmu = NA, hatsd = NA, hatcv = NA )
lismu <- list()

for( j in 1:length(stra)){
x <- dt_lp[dt_lp$Clase == stra[j],]
x$id <- 1:nrow(x)

samn <- round(nrow(x)*.4)
mu <- NA
row_out <- data.frame(prob = NA ,hatmu = NA ,hatsd = NA ,hatcv =NA)

for( i in 1:boon){
  
  xs <- x[sample(x$id,samn,replace = T),]
  mu0 <- sum(xs$FEX_Cx,na.rm = T)
  mu <- c(mu,mu0)
  mu <- mu[complete.cases(mu)]
}

lismu[[j]] <- data.frame(mu)

hatmu <- round(mean(mu,na.rm = T),4)
hatsd <- round(sd(mu, na.rm = T),4)
hatcv <- round((hatsd/hatmu) * 100,4)

}
lismu[[1]]
lismu[[2]]
# ** b.1.1 estimación punto ------------------------------------------------------------------------

dtout_1 <- doBy::summary_by(FEX_C  ~ P1672, data = dt_lp1, FUN = sum )
dtout_2 <- merge(dtout,dtout_1, all.x = T, by.y = 'P1672', by.x = 'problema')
openxlsx::write.xlsx(dtout_2,'nj_mujeres_delitos.xlsx')


# * d.3.histogramas --------------------------------------------------------------------------------
str(lismu)
names(lismu_salu) <- unique(dt_ln_salu$P3013_lab)

all <- do.call("rbind", lismu)
all$id <- rep(names(lismu), sapply(lismu, nrow))

all <- all %>%
  summarise(Mean = mean(mu))
sd <- all %>%
  summarise(Mean = sd(mu))
sd/mu


all$id <- stringr::str_wrap(all$id, width = 40)

x <- ggplot(all, aes(x = (mu)/1000, fill = as.factor(id))) +
  geom_histogram(bins = 80) +
  scale_fill_viridis_d( ) +
  facet_wrap( ~ id, scales = "free") + theme_ipsum() +
  theme(legend.position = 'None')+
  xlab("Necesidades jurídicas en miles")+
  theme(panel.background = element_blank()) +
  ylab("Conteo")

ggsave(plot = x, file = "graph2.png", 
       type = "cairo-png",  bg = "transparent")

# e. dist plot -------------------------------------------------------------------------------------

mudist <- do.call(cbind.data.frame, lismu)
names(mudist) <-   gsub("[^0-9.-]", "",substr(stra,1,3))
mudist$id <- 1:nrow(mudist)
names(mudist) <- c('salud','familia',
                   'estado','educación',
                   'discriminación',
                   'delitos','conflicto',
                   'consumo','servPublicos',
                   'empleo','deudas','vivienda',
                   'entorno','propiedad','medio ambiente','id')

library(ggplot2)
library(ggridges)
library(tidyr)

keycol <- "problema"
valuecol <- "mu"
gathercols <- names(mudist)[1:15]

mudistlong <- gather_(mudist, keycol, valuecol, gathercols)


theme_set(theme_minimal())
mudistlong$mul <- log(mudistlong$mu)

ggplot(
  mudistlong, 
  aes(x = `mul`, y = `problema`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 10, size = 0, rel_min_height = 0.001) +
  scale_fill_viridis_c(name = "Logaritmo de fexc", option = "magma") +
  labs(title = 'Distribución de NJ declaradas sobre muestras del 40% en grupos de problemas')



cat_labs

dt <- data.frame(script = NA)

var <- unique(cat_labs$var)


i <- 1

for(i in 1:length(var)){
a <- cat_labs[cat_labs$var == var[i],]
v <- paste(a$a, "\"",a$b,"\"",collapse = "")
v <- paste('label define', var[i],v)

dt[i, 'script'] <- v
}

openxlsx::write.xlsx(file = 'script_labelsSTATA.xlsx',dt)

dt1 <- data.frame(script = NA)

var <- unique(cat_labs$var)


i <- 1

for(i in 1:length(var)){
  
  v <- paste('label values', var[i],var[i])
  
  dt1[i, 'script'] <- v
}

openxlsx::write.xlsx(file = 'script_labelsSTATA2.xlsx',dt1)

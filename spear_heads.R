nuevo_dir <- 'C:/spear_heads'
setwd(nuevo_dir) 

install.packages("readxl")
library(readxl)

#1
spear = read_excel('C:/spear_heads/spearheads.xlsx')
View(spear)
str(spear)

class(spear)

spear <- as.data.frame(spear)

#2 
names(spear)[names(spear) == "Mat"] <- "Materiales"
names(spear)[names(spear) == "Con"] <- "Contexto"
names(spear)[names(spear) == "Cond"] <- "Conservación"
names(spear)[names(spear) == "Loo"] <- "Loop"
names(spear)[names(spear) == "Peg"] <- "Remache"
names(spear)[names(spear) == "Date"] <- "Fecha"
names(spear)[names(spear) == "Maxle"] <- "Longitud_max"
names(spear)[names(spear) == "Socle"] <- "Longitud_encaje"
names(spear)[names(spear) == "Maxwi"] <- "Ancho_max"
names(spear)[names(spear) == "Upsoc"] <- "Ancho_encaje"
names(spear)[names(spear) == "Maxwit"] <- "Ancho_max_encaje"
names(spear)[names(spear) == "Weight"] <- "Peso"
View(spear)

#3
spear$Contexto=factor(spear$Contexto, levels=c('1','2','3'), labels = c('S/C', 'Habitacional', 'Funerario'))
spear$Conservación=factor(spear$Conservación, levels=c('1','2','3','4'), labels = c('Excelente', 'Bueno', 'Regular', 'Malo'))
spear$Remache=factor(spear$Remache, levels=c('1','2'), labels = c('Sí', 'No'))
spear$Materiales=factor(spear$Materiales, levels=c('1','2'), labels = c('Bronce', 'Hierro'))

#4
freq.mat = table(spear$Materiales)
freq.con = table(spear$Contexto)
freq.cond = table(spear$Conservación)

#5
cross.condcon = table(spear$Materiales,spear$Contexto)
cross.condmat = table(spear$Materiales,spear$Conservación)

#6
prop.mat = prop.table(freq.mat)*100
prop.con = prop.table(freq.con)*100
prop.cond = prop.table(freq.cond)*100

#7
prop.cross.propcond = round(prop.table(cross.condcon)*100)
prop.cross.propmat = round(prop.table(cross.condmat)*100)

#8
graf_vert_cond = barplot(freq.cond, 
                         main = "Frecuencia",
                         xlab = "Grado",
                         ylab = "Porcentaje",
                         col = "red")

graf_vert_con = barplot(freq.con, 
                        main = "Frecuencia",
                        xlab = "Grado",
                        ylab = "Porcentaje",
                        col = "blue")

#9
graf_hor_mat = barplot(freq.mat, horiz = TRUE,
                       main = "Frecuencia",
                       xlab = "Porcentaje",
                       ylab = "Grado",
                       col = "khaki")


freq.rem = table(spear$Remache)
barras_hor_rem = barplot(freq.rem, horiz = TRUE,
                         main = "Frecuencia",
                         xlab = "Porcentaje",
                         ylab = "Grado",
                         col = "black")

#10
barplot(cross.condmat,
        beside = TRUE,
        main = "Conservación sobre el material",
        xlab = "Grado",
        ylab = "Porcentaje",
        col = c("red","blue"),
        legend = rownames(cross.condmat))

#11
pie(prop.cond,
    main = "Distribución de los grados de conservación",
    col = c("blue","pink","purple","red"),
    labels = paste(names(prop.cond),"(", (prop.cond),"%)"))

#12
variables_continuas = spear[sapply(spear, is.numeric)]
windows(width = 10, height = 10)

hist_prob = hist(unlist(variables_continuas), 
                 main = "Histograma de las Variables Continuas",
                 xlab = "Valores",
                 col = "black",
                 prob = TRUE)
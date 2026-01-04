

# Loading the necessary packages
library(tidyverse)
library(ggplot2)
library(ggplot2)
library(gridExtra)

datos <- read.csv("D:/José Luis/UPC/TFM/Lake/BdD Clinical Trial/LAKE/lake.csv", sep=";",dec = ",", header = T ,encoding = "UTF-8",stringsAsFactors = FALSE,)
head(datos)
#datosp <- select(datosp, -c("week"))

str(datos)
summary(datos)

#Preparing panel data (nusuario-npac)

datos$id <- cumsum(!duplicated(datos[2:3]))
datos <- datos %>% relocate(id)
str(datos$week_0)
str(datos$week_12)
str(datos$week_24)
str(datos$week_36)
str(datos$week_48)
sum(datos$CD4A_0,na.rm=T)
sum(datos$CD4A_12,na.rm=T)

#  datos <- datos %>%
 # mutate(week_36 = ifelse(week_36 == 5, "36", week_36),
  #       week_48 = ifelse(week_48 == 6, "48", week_48))

"converting data from wide to long"

#datosp<-reshape(data.frame(datos), direction='long', 
 #          varying=c('week_0','Fecha_0','CargaViral_0','CD4A_0','CD4P_0','CD8A_0','CD8P_0','Hematocrito_0','Hemoglobina_0','Plaquetas_0','Leucocitos_0','LinfosTotales_0','Glucosa_mg_0','Urea_mg_0','Creatinina_mumol_0','Sodio_0','Potasio_0','Cloro_0','Calcio_0','Bilirrubina_mumol_0','GPT_0','GOT_0','GGT_0','ProteinasTotales_0','Albumina_0','Colesterol_mg_0','LDL_mg_0','HDL_mg_0','Trigliceridos_mg_0','Amilasa_0','pH_0','Bicarbonato_0','AcidoLactico_0','AcidoPiruvico_0','VHC_0','VHB_0','Embarazo_0','week_12','Fecha_12','CargaViral_12','CD4A_12','CD4P_12','CD8A_12','CD8P_12','Hematocrito_12','Hemoglobina_12','Plaquetas_12','Leucocitos_12','LinfosTotales_12','Glucosa_mg_12','Urea_mg_12','Creatinina_mumol_12','Sodio_12','Potasio_12','Cloro_12','Calcio_12','Bilirrubina_mumol_12','GPT_12','GOT_12','GGT_12','ProteinasTotales_12','Albumina_12','Colesterol_mg_12','LDL_mg_12','HDL_mg_12','Trigliceridos_mg_12','Amilasa_12','pH_12','Bicarbonato_12','AcidoLactico_12','AcidoPiruvico_12','VHC_12','VHB_12','Embarazo_12','week_24','Fecha_24','CargaViral_24','CD4A_24','CD4P_24','CD8A_24','CD8P_24','Hematocrito_24','Hemoglobina_24','Plaquetas_24','Leucocitos_24','LinfosTotales_24','Glucosa_mg_24','Urea_mg_24','Creatinina_mumol_24','Sodio_24','Potasio_24','Cloro_24','Calcio_24','Bilirrubina_mumol_24','GPT_24','GOT_24','GGT_24','ProteinasTotales_24','Albumina_24','Colesterol_mg_24','LDL_mg_24','HDL_mg_24','Trigliceridos_mg_24','Amilasa_24','pH_24','Bicarbonato_24','AcidoLactico_24','AcidoPiruvico_24','VHC_24','VHB_24','Embarazo_24','cv50_0','cv50_12','cv50_24','week_36','Fecha_36','CargaViral_36','CD4A_36','CD4P_36','CD8A_36','CD8P_36','Hematocrito_36','Hemoglobina_36','Plaquetas_36','Leucocitos_36','LinfosTotales_36','Glucosa_mg_36','Urea_mg_36','Creatinina_mumol_36','Sodio_36','Potasio_36','Cloro_36','Calcio_36','Bilirrubina_mumol_36','GPT_36','GOT_36','GGT_36','ProteinasTotales_36','Albumina_36','Colesterol_mg_36','LDL_mg_36','HDL_mg_36','Trigliceridos_mg_36','Amilasa_36','pH_36','Bicarbonato_36','AcidoLactico_36','AcidoPiruvico_36','VHC_36','VHB_36','Embarazo_36','week_48','Fecha_48','CargaViral_48','CD4A_48','CD4P_48','CD8A_48','CD8P_48','Hematocrito_48','Hemoglobina_48','Plaquetas_48','Leucocitos_48','LinfosTotales_48','Glucosa_mg_48','Urea_mg_48','Creatinina_mumol_48','Sodio_48','Potasio_48','Cloro_48','Calcio_48','Bilirrubina_mumol_48','GPT_48','GOT_48','GGT_48','ProteinasTotales_48','Albumina_48','Colesterol_mg_48','LDL_mg_48','HDL_mg_48','Trigliceridos_mg_48','Amilasa_48','pH_48','Bicarbonato_48','AcidoLactico_48','AcidoPiruvico_48','VHC_48','VHB_48','Embarazo_48','cv50_36','cv50_48'), 
  #         timevar='semana',
   #        times=c(0, 12, 24, 36, 48),
    #       v.names=c('week','Fecha','CargaViral','CD4A','CD4P','CD8A','CD8P','Hematocrito','Hemoglobina','Plaquetas','Leucocitos','LinfosTotales','Glucosa_mg','Urea_mg','Creatinina_mumol','Sodio','Potasio','Cloro','Calcio','Bilirrubina_mumol','GPT','GOT','GGT','ProteinasTotales','Albumina','Colesterol_mg','LDL_mg','HDL_mg','Trigliceridos_mg','Amilasa','pH','Bicarbonato','AcidoLactico','AcidoPiruvico','VHC','VHB','Embarazo','cv50 '),
     #      idvar='id', sep = "_")

library(data.table)
times <- gsub("week_", "", grep("week_", names(datos), value = TRUE))
datosp<-melt(as.data.table(datos), measure.vars = patterns('week_','Fecha_','CargaViral_','CD4A_','CD4P_','CD8A_','CD8P_','Hematocrito_','Hemoglobina_','Plaquetas_','Leucocitos_','LinfosTotales_','Glucosa_mg_','Urea_mg_','Creatinina_mumol_','Sodio_','Potasio_','Cloro_','Calcio_','Bilirrubina_mumol_','GPT_','GOT_','GGT_','ProteinasTotales_','Albumina_','Colesterol_mg_','LDL_mg_','HDL_mg_','Trigliceridos_mg_','Amilasa_','pH_','Bicarbonato_','AcidoLactico_','AcidoPiruvico_','VHC_','VHB_','Embarazo_','cv50_'),
             value.name = c('week','Fecha','CargaViral','CD4A','CD4P','CD8A','CD8P','Hematocrito','Hemoglobina','Plaquetas','Leucocitos','LinfosTotales','Glucosa_mg','Urea_mg','Creatinina_mumol','Sodio','Potasio','Cloro','Calcio','Bilirrubina_mumol','GPT','GOT','GGT','ProteinasTotales','Albumina','Colesterol_mg','LDL_mg','HDL_mg','Trigliceridos_mg','Amilasa','pH','Bicarbonato','AcidoLactico','AcidoPiruvico','VHC','VHB','Embarazo','cv50 '))[
               , variable := factor(variable, labels = times)][]


#Ordering data by id and week

datosp <- select(datosp, -c("week"))
colnames(datosp)[colnames(datosp) == "variable"] <- "week"
datosp <- datosp[order(datosp$id, datosp$week), ]
library(plm)
str(datosp)
#Convert Specific Columns to Numeric
library(dplyr)
#datosp <- datosp %>% mutate_at(c(24:68), as.numeric)
datosp$Grupo[datosp$Grupo== -1] <- 1
datosp$Grupo <- as.factor(datosp$Grupo)

######convertir a panel data
datosp<-pdata.frame(datosp,index = c("id","week"))

#CD4A outcome
#In the lcmm examples, MMSE is usually considered as the outcome. MMSE is a very common neuropsychological test to measure global cognitive functioning in the elderly. It has a very asymmetric distribution so that it is usually normalized to be applied with methods for Gaussian variables. This is done using a pre-normalization function dedicated to MMSE provided in NormPsy package:
#datosp$normCD4A <- scale(datosp$CD4A, center = TRUE, scale = TRUE)

#variable Transformation
datosp$normCD4A <- sqrt(datosp$CD4A)
#datosp$normCD4A <- log(datosp$CD4A)

par(mfrow=c(1,2))
hist(datosp$CD4A, cex.main=0.8, cex.lab=0.8)
hist(datosp$normCD4A, cex.main=0.8, cex.lab=0.8)

#CD4A boxplot by treatment (Grupo)
boxplot(normCD4A~Grupo, data=datosp,col='cadetblue',main='CD4A boxplot by treatment')

hist(datosp$normCD4A, main='Histogram of CD4A ', xlab = 'CD4A')
#CD4A evolution by treatment and viral load

ggplot(data = datosp)+geom_point(aes(x = week, y = normCD4A, color=Grupo, size= CargaViral))#The later repetitions have the smaller break angles.

#CD4A spaghetti plot 
library(ExPanDaR)
library(epiDisplay)
g<-ggplot(data=datosp,aes(x = week, y = normCD4A,group=id))
g+
  geom_line(aes())+
  theme(panel.grid = )

#CD4A spaghetti plot by treatment
g<-ggplot(data=datosp,aes(x = week, y = normCD4A,group=id))
g+
  geom_line(aes())+
  theme(panel.grid = )+
  facet_wrap(~Grupo) 

#continuar
#quick look at the distribution.
prepare_by_group_violin_graph(datosp, by_var = "week", var = "normCD4A",
                              order_by_mean = TRUE)

prepare_scatter_plot(datosp, x="week", y="normCD4A", color="Grupo", size="CargaViral", loess = 1)

## Covariance and correlation structure
library(corrplot)
matrix<-cor(na.omit(datosp[,34:69]))
corrplot(matrix,method = "number")

# Identificar variables constantes (numéricas y excluyendo factores)
#constant_vars <- sapply(datosp, function(x) {
#  if (is.numeric(x)) {
#    sd(x, na.rm = TRUE) == 0
#  } else {
#    FALSE
#  }
#})

#The individual repeated measures of MMSE to be modelled are:

library(lattice)
color <- datosp$id
xyplot(CD4A ~ week, datosp, groups = id, col=color, lwd=2, type="l")
xyplot(normCD4A ~ week, datosp, groups = id, col=color, lwd=2, type="l")

#LCMM
datosp$normCD4A<-as.numeric(datosp$normCD4A)
datosp$CD4A<-as.numeric(datosp$CD4A)
datosp$id<-as.numeric(datosp$id)
datosp$week<-as.numeric(datosp$week)
str(datosp$CD4A)
str(datosp$week)
str(datosp$id)

library(lcmm)
nrow(datosp)
length(datosp$normCD4A)
length(datosp$CD4A)
length(datosp$week)
length(datosp$id)
length(datosp$edad)
#########
# Crear un conjunto de datos de ejemplo
#set.seed(123)
#datos <- data.frame(
#  ID = rep(1:100, each = 5),
# time = rep(1:5, times = 100),X = rnorm(500),
#  Y = rnorm(500)  # Variable de interés
#)
#########

#datosp$normCD4A<-datosp[!is.na(datosp$normCD4A),]
#datosp$Grupo<-datosp[!is.na(datosp$Grupo),]
#datosp$week<-datosp[!is.na(datosp$week),]
#datosp$id<-datosp[!is.na(datosp$id),]
#datosp$CD4A<-datosp[!is.na(datosp$CD4A),]
#datosp$normCD4A<-datosp[!is.na(datosp$normCD4A),]
#datosp <- datosp[complete.cases(datosp$normCD4A), ]
#datosp <-datosp %>% drop_na(normCD4A)

#https://cecileproust-lima.github.io/lcmm/articles/latent_class_model_with_hlme.html
#Estimate the model with only one class (G=1)

str(datosp$week)
str(datosp$id)
#m1 <- hlme(Y ~ time+I(time^2)+X,random =~ time+I(time^2), subject = 'ID', data = datos) # ng=1
#m1 <- lcmm(CD4A ~ week,random =~ week|id , subject = 'id', data = datosp) # ng=1
m1 <- lcmm(normCD4A ~ week,random =~ week|id , subject = 'id', data = datosp) # ng=1

str(datosp$CD4A)
str(datosp$week)
str(datosp$id)
str(datosp$normCD4A)
summary(datosp)
#Estimation considering 2 classes : 
#m2 <- hlme(Y ~ time+I(time^2)+X, random =~ time+I(time^2), subject = 'ID', data = datos, ng = 2, mixture=~time+I(time^2), B=m1)
#m2d <- gridsearch(hlme(Y ~ time+I(time^2)+X,  random =~ time+I(time^2), subject = 'ID', data=datos, ng = 2, mixture=~time+I(time^2)), rep=100, maxiter=30, minit=m1)
#m3g <- gridsearch(hlme(Y ~ time+I(time^2)+X,  random =~ time+I(time^2), subject = 'ID', data=datos, ng = 3, mixture=~time+I(time^2)), rep=100, maxiter=30, minit=m1)

m2 <- lcmm(normCD4A ~ week, random =~ week|id, subject = 'id', data = datosp, ng = 2, mixture=~week, B=m1)
m2b <- lcmm(normCD4A ~ week, random =~ week|id, subject = 'id', data = datosp, ng = 2, mixture=~week,B = c(2, 60, 40, 4, 3, 10, 212.869397, 216.421323, 10.072221))
m2c <- lcmm(normCD4A ~ week, random =~ week|id, subject = 'id', data = datosp, ng = 2, mixture=~week, B=random(m1))
m2d <- gridsearch(lcmm(normCD4A ~ week,  random =~ week|id, subject = 'id', data=datosp, ng = 2, mixture=~week), rep=50, maxiter=10, minit=m1)
m3g <- gridsearch(lcmm(normCD4A ~ week ,  random =~ week|id, subject = 'id', data=datosp, ng = 3, mixture=~week), rep=50, maxiter=10, minit=m1)

summarytable(m1,m2,m2b,m2c,m2d,m3g, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy","ICL", "%class"))
summaryplot(m1, m2,m2b,m2c, m3g, which = c("BIC", "entropy","ICL"))
summary(m2d)
postprob(m2d)
summary(m2c)
summary(m3g)

data_pred0 <- data.frame(week=seq(0,50,length.out=50),id=seq(0,50,length.out=50))
data_pred1 <- data.frame(week=seq(0,50,length.out=50),id=seq(0,50,length.out=50))
#data_pred0$Grupo <- (data_pred0$Grupo - 65)/10
#data_pred1$Grupo <- (data_pred1$Grupo - 65)/10

pred0 <- predictY(m2c, data_pred0, var.time = "week")
pred1 <- predictY(m2c, data_pred1, var.time = "week")

plot(pred0, col=c("red","navy"), lty=1,lwd=5,ylab="normCD4A",legend=NULL,  main="Predicted trajectories for CD4A",ylim=c(-300,200))
plot(pred1, col=c("red","navy"), lty=2,lwd=3,legend=NULL,add=TRUE)
legend(
  x = "topright",
  legend = c("class1:", "class2:"),
  col = c("red", "navy"),
  lwd = 2,
  lty = c(1, 1),  # Ajustar a lty=2 para el segundo gráfico
  ncol = 2,
  bty = "n",
  cex = 0.7
)
#legend(x="topright",legend=c("class1 :","Grupo-","Grupo+","class2:","Grupo-","Grupo+"), col=c(rep("red",3),rep("navy",3)), lwd=2, lty=c(0,1,2,0,1,2), ncol=2, bty="n", cex = 0.7)

predIC0 <- predictY(m2c, data_pred0, var.time = "week",draws=TRUE)
predIC1 <- predictY(m2c, data_pred1, var.time = "week",draws=TRUE)
plot(predIC0, col=c("deeppink","deepskyblue"), lty=1, lwd=2, ylab="normCD4A", main="Predicted trajectories for CD4A", ylim=c(-300,200), shades=TRUE)
plot(predIC1, col=c("deeppink","deepskyblue"), lty=2, lwd=2, ylab="normCD4A", main="Predicted trajectories for CD4A", legend=NULL, ylim=c(-300,100), shades=TRUE, add=TRUE)

predG1 <- predictY(m1, data_pred0, var.time = "week")
predG3 <- predictY(m3g, data_pred0, var.time = "week")

par(mfrow=c(1,3))
plot(predG1, col=1, lty=1, lwd=2, ylab="CD4A", legend=NULL, main="Predicted trajectories G=1",ylim=c(-30,100))
legend("topright", legend=c("Class 1", "95% CI"), 
       col=c("black", "grey"), lty=c(1, NA), fill=c(NA, "grey"), 
       border=NA, bty="n", cex=0.8)
plot(pred0, col=c("red","navy"), lty=1, lwd=2,ylab="CD4A", legend=NULL, main="Predicted trajectories G=2", ylim=c(-200,100))
legend("topright", legend=c("Class 1", "Class 2", "95% CI"), 
       col=c("red", "navy", "grey"), lty=c(1, 1, NA), 
       fill=c(NA, NA, "grey"), border=NA, bty="n", cex=0.8)
plot(predG3, col=2:4, lty=1, lwd=2, ylab="CD4A", legend=NULL, main="Predicted trajectories G=3", ylim=c(-200,100))
legend("topright", legend=c("Class 1", "Class 2", "Class 3", "95% CI"), 
       col=c("red", "navy", "green", "grey"), lty=c(1, 1, 1, NA), 
       fill=c(NA, NA, NA, "grey"), border=NA, bty="n", cex=0.8)

#Evaluation of the final latent class mixed model
#Plot of the residuals
plot(m2c, cex.main=0.8)
plot(m3g, cex.main=0.8)

#Graph of the predictions versus observations
plot(m2c, which="fit", var.time="week", marg=F,ylim=c(-10,10), points=T, shades = T, legend=NULL,col=1,lty=1,lwd=1,xlab = "Week", ylab = "CD4A")
plot(m3g, which="fit", var.time="week", marg=F,ylim=c(-10,30), points=T, shades = T, legend=NULL,col=1,lty=1,lwd=1,xlab = "Week", ylab = "CD4A")

#Classification
#The posterior classification of the model is obtained with:
postprob(m2c)
postprob(m3g)

round(summary(as.numeric(m2d$pprob[m2d$pprob[,"class"]==1,"prob1"])),2)
round(summary(as.numeric(m2d$pprob[m2d$pprob[,"class"]==2,"prob2"])),2)

library(lattice)
color <- datosp$id
xyplot(normCD4A ~ week, datosp, groups = id, col=color, lwd=2, type="l")

library(dplyr)

# trends and trajectories
#https://www.r-bloggers.com/2011/10/latent-class-mixed-models-with-graphics/

#install.packages(c("ggplot2", "gridExtra"), dependencies = TRUE)
library(gridExtra)
library(ggplot2)

#datosp %>%
#  select(id, week)

datosp<-datosp %>%
  filter(!id == 109)
datosp <- as.data.frame(datosp)

#2classes
datosp$id <- as.character(datosp$id)
classg2 <- as.data.frame(m2c$pprob[,1:2])
datosp$groups2 <- factor(classg2$class[sapply(datosp$id, function(x) which(classg2$id==x))])
p1 <- ggplot(datosp, aes(week, normCD4A, group=id, colour=groups2)) + geom_line() + geom_smooth(aes(group=groups2), method="loess", size=2, se=F)  + scale_y_continuous(limits = c(0,40)) + labs(x="week",y="normCD4A",colour="Latent Class") + ggtitle('Raw')
p2 <- ggplot(datosp, aes(week, normCD4A, group=id, colour=groups2)) + geom_smooth(aes(group=id, colour=groups2),size=0.5, se=F) + geom_smooth(aes(group=groups2), method="loess", size=2, se=T)  + scale_y_continuous(limits = c(0,40)) + labs(x="week",y="normCD4A",colour="Latent Class") + ggtitle('Smoothed') + theme(legend.position = 'none')
grid.arrange(grobs=list(p1,p2), ncol=2, main="2 Latent Class")

#3classes
datosp$id <- as.character(datosp$id)
classg3 <- as.data.frame(m3g$pprob[,1:2])
datosp$groups3 <- factor(classg3$class[sapply(datosp$id, function(x) which(classg3$id==x))])
p1 <- ggplot(datosp, aes(week, normCD4A, group=id, colour=groups3)) + geom_line() + geom_smooth(aes(group=groups3), method="loess", size=2, se=F)  + scale_y_continuous(limits = c(0,40)) + labs(x="week",y="normCD4A",colour="Latent Class") + ggtitle('Raw')
p2 <- ggplot(datosp, aes(week, normCD4A, group=id, colour=groups3)) + geom_smooth(aes(group=id, colour=groups3),size=0.5, se=F) + geom_smooth(aes(group=groups3), method="loess", size=2, se=T)  + scale_y_continuous(limits = c(0,40)) + labs(x="week",y="normCD4A",colour="Latent Class") + ggtitle('Smoothed') + theme(legend.position = 'none')
grid.arrange(grobs=list(p1,p2), ncol=2, main="2 Latent Class")

length(datosp$id)
length(classg3$class)
length(classg3$id)

#str(datosp$week)
#str(datosp$CD4A)
#str(datosp$id)
#str(datosp$groups)
#any(is.na(datosp$CD4A) | is.infinite(datosp$CD4A))


#datosp$week <- as.numeric(datosp$week)
#datosp$groups <- as.numeric(datosp$groups)
#datosp$groups <- factor(datosp$groups)

####Comparing two groups
t.test(normCD4A ~ groups2, data = datosp, paired = F)

# Realizar un ANOVA para comparar medias entre grupos
modelo_anova <- aov(normCD4A ~ groups2, data = datosp)

# Imprimir resultados del ANOVA
print(summary(modelo_anova))

#Summary statistics by groups
datosp %>% group_by(groups2) %>%  summarise(n = n(), mean = mean(normCD4A, na.rm = TRUE), sd = sd(normCD4A, na.rm = TRUE))

#Generate boxplot to check data spread
ggplot(datosp, aes(x = groups2, y = normCD4A, col = groups2)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")

#Check data distribution
shapiro.test(datosp$normCD4A)

# plot histogram
ggplot(datosp, aes(x = normCD4A)) + geom_histogram(aes(y=..density..), colour="black", fill="skyblue", binwidth = 10) + 
  geom_density(color="red", size=1)

#Check homogeneity of variances assumption
library(car)
leveneTest(normCD4A ~ groups2, data=datosp)

#Perform Kruskal-Wallis test
kruskal.test(normCD4A ~ groups2, data = datosp)

# calculate effect size
library(rcompanion)

epsilonSquared(x = datosp$normCD4A, g = datosp$groups2)

 #post-hoc test
#library(FSA)
#dunnTest(normCD4A ~ groups2, data = datosp, method = "bh")

#Conover test
library(PMCMRplus)
datosp$groups2 = as.factor(datosp$groups2)

# Tukey's p-adjustment (single-step method)
kwAllPairsConoverTest(normCD4A ~ groups2, data = datosp)

#Nemenyi test
# Nemenyi test
# Tukey's p-adjustment (single-step method)
kwAllPairsNemenyiTest(normCD4A ~ groups2, data = datosp)

####Comparing three groups
#https://www.datanovia.com/en/blog/how-to-perform-t-test-for-multiple-groups-in-r/

# Load required R packages
library(tidyverse)
library(rstatix)
library(ggpubr)

datosp %>% sample_n_by(groups3, size = 1)

#Summary statistics
datosp %>%
  group_by(groups3) %>%
  get_summary_stats(normCD4A, type = "mean_sd")

#Compare the mean of multiple groups using ANOVA test
res.aov <- datosp %>% anova_test(normCD4A ~ groups3)
res.aov

# Pairwise comparisons
pwc <- datosp %>%
  pairwise_t_test(normCD4A ~ groups3, p.adjust.method = "bonferroni")
pwc

#Visualization: box plots with p-values

# Show adjusted p-values
pwc <- pwc %>% add_xy_position(x = "groups3")
ggboxplot(datosp, x = "groups3", y = "normCD4A") +
  stat_pvalue_manual(pwc, label = "p.adj", tip.length = 0, step.increase = 0.1) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# Show significance levels
# Hide non-significant tests
ggboxplot(datosp, x = "groups3", y = "normCD4A") +
  stat_pvalue_manual(pwc, hide.ns = TRUE, label = "p.adj.signif") +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


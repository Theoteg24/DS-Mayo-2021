library(dplyr)
library(visdat)
library(naniar)
library(VIM)
library(BBmisc)
library(discretization)
library(smoothmest)
library(readxl)


library("naniar")
library("nortest")
library("VIM")
library("simputation")
library(dummies)
library(fastDummies)
library(tidyverse)
#1. Carga la tabla en R y confirma que los formatos se adecúan a los esperados para las variables que presenta el dataset. En caso contrario, corrige!
Datos <- read_excel("C:/Users/alvar/Downloads/SBAnational.xlsx")

View(SBAnational)
str(Datos)

Datos$ApprovalDate=as.Date(Datos$ApprovalDate)
Datos$disbursementdate=as.Date(Datos$disbursementdate)
Datos$ChgOffDate=as.Date(Datos$ChgOffDate)
Datos$City=as.factor(Datos$City)
Datos$State=as.factor(Datos$State)
Datos$Bank=as.factor(Datos$Bank)
Datos$BankState=as.factor(Datos$BankState)
Datos$ApprovalFY=as.factor(Datos$ApprovalFY)
Datos$RevLineCr=as.factor(Datos$RevLineCr)
Datos$LowDoc=as.factor(Datos$LowDoc)
Datos$MIS_Status=as.factor(Datos$MIS_Status)
#2. Anonimiza mediante hasheo el identificador de la persona que pide el préstamo. Justifica la función utilizada

Datos <-Datos %>%
  mutate(var=C(1:length(Datos)))

#He utilizado esta funcion principalmente debido a que sustituir los valores a anonimizar de un data frame por un número consecutivo es sencillo a la vez que efectivo

#2. Analiza de forma global la calidad de los datos presentados: 
#detecta valores fuera de rango para las variables discretas, missings y outliers 
#para las numéricas(*) continuas. Los outliers detectalos con métodos gráficos
## estadisticos generales
estadisticos_vars_cont <- as.data.frame(summary(Datos))

#NAs
sum(is.na(Datos))

#vamos a utilizar un sample

muestra<-sample(Datos, 4)
vis_miss(muestra, cluster = TRUE, warn_large_data = FALSE)

misings_por_columnas<-as.data.frame(miss_var_summary(Datos))
View(misings_por_columnas)

misings_por_filas<-as.data.frame(miss_case_summary(Datos))
View(misings_por_filas)

miss_case_summary(Datos)
miss_var_summary(Datos)

gg_miss_var(Datos)

vis_miss(Datos, cluster=TRUE,warn_large_data = FALSE) 
View(Datos)


#Outliers
boxplot(Datos$SBA_Appv)
lillie.test(Datos$SBA_Appv)

boxplot(Datos$SBA_Appv, plot=FALSE)$out
outliers <- boxplot(Datos$SBA_Appv, plot=FALSE)$out


p=boxplot(as.data.frame(Datos[,26:27]))
p=boxplot(as.data.frame(Datos[,24]))
p=boxplot(as.data.frame(Datos[,23]))
p=boxplot(as.data.frame(Datos[,12:18]))
#3. Completa los valores ausentes explícitos que te encuentres de las variables 
#de tipo continuo, justifcando la métrica seleccionada para hacerlo
#Ademas gracias a los graficos me he dado cuenta que BalanceGross siempre es 0 por lo  que es una variable que sobra

o=for(i in (23)) {Datos <- kNN(Datos, colnames(Datos)[i], dist_var=c("Bank", "GrAppv"))}
View(o)
View(Datos)
#5. Anonimiza la consulta de BalanceGross medio por ZIP con un epsilon de 0.1
#Es un metodo sencillo pero a la vez eficaz ya que realmente no se sabe asi los datos que se anonimizan
#Media
BalanceGross_media <- Datos %>%
  summarize(BalanceGross_media=mean(BalanceGross))

epsilon=0.1
a<-min(Datos$BalanceGross)
b<-max(Datos$BalanceGross)
n<-nrow(Datos)
gs <- (b-a)/n 

dim(Datos)
View(WHR)
BalanceGross_media_anon <- round(rdoublex(94765, BalanceGross_media, gs/epsilon),2)
View(Generosity_media_anon)

#7. Transforma la variable UrbanRural en variables dummies. ¿Cuántas dummies son necesarias para no perder información?
w=dummy(Datos[,17])
View(w)
#La transformacion de la variable Urban Rural esta hecha, por otro lado si no queremos perder informacion yo añadiria las variables que tengo seleccionadas como categoricas
#He cogido todas la variables factor con las que se podia convertir a dummy


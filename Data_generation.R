library(tidyverse)
library(haven)
library(gtsummary)
library(survey)


#1. Preparacion de datos

# Bases por anio
#He unido los archivos de cada modulo para tener una sola base por anio. Algunas encuestas son dirigidas a la mujer en edad fertil/madre, otras nivel hogar. Como veran, solo la base de programas sociales es a nivel hogar (de las que he usado), por eso es la unica que el join es por HHID. Las demas, son dirigidas a la madre/MEF. 

## Gestantes 2020

gestacion1 <- read_sav("./data/raw/endes back up/2020/RE223132.sav")
gestacion2 <- read_sav("./data/raw/endes back up/2020/REC94.sav") # outcome # id
violence <- read_sav("./data/raw/endes back up/2020/REC84DV.sav")
sociodemo <- read_sav("./data/raw/endes back up/2020/REC0111.sav") %>% select(-NCONGLOME)
vih1 <- read_sav("./data/raw/endes back up/2020/RE758081.sav")
vih2 <- read_sav("./data/raw/endes back up/2020/REC91.sav")
nacion <- read_sav("./data/raw/endes back up/2020/RECH1.sav") #HHID
prenatal <- read_sav("./data/raw/endes back up/2020/REC41.sav")
seguro <- read_sav("./data/raw/endes back up/2020/RECH4.sav") #HHID

#SelecciónVariables
gestacion1_v <- gestacion1 %>% select(ID1, CASEID, V201, V208, V211)
gestacion2_v <- gestacion2 %>% select(ID1, CASEID, S413, S411G, S411H, QI411_M)
violence_v <- violence %>% select(ID1, CASEID, D118Y, V005V)
sociodemo_v <- sociodemo %>% select(ID1, CASEID, V012, V190, V150, V025, V103, V136, V131, V024, V155, V001, V005, V022)
vih1_v <- vih1 %>% select(ID1, CASEID, V763A, V763B, V763C, V774A, V774B, V774C)
vih2_v <- vih2 %>% select(ID1, CASEID, S815AC, S815AD, S815AE, S815AX, S815AZ, S816AA, S816AB, S816AC, S816AD, S816AE, S816AF,
                S816AG, S816AH, S816AI, S816AJ, S816AK, S816AL, S816AW, S816AZ)
nacion_v<-nacion %>% select(ID1, HHID, HV115, QH25A)
prenatal_v <- prenatal %>% select(ID1, CASEID, M2A, M2B, M2C, M2D, M2E, M2F, M2G, M2H, M2I, M2J, M2K, M2L, M2M, M2N, M10, M13,
                 M14, M57E, M57F, M57G, M57H, M57I, M57J, M57K, M57L, M57M, M57N, M57O, M57P, M57Q, M57R, M57S,
                 M57T, M57U, M57V, M57X)
seguro_v <- seguro %>% select(ID1, HHID, SH11A, SH11B, SH11C, SH11D, SH11E, SH11Y, SH11Z)

# UnionData
df_gestantes2020<-
  gestacion2_v %>% 
  left_join(gestacion1_v, by = "CASEID") %>% 
  left_join(violence_v, by = "CASEID") %>% 
  left_join(sociodemo_v, by = "CASEID") %>%
  left_join(vih1_v, by = "CASEID") %>% 
  left_join(vih2_v, by = "CASEID") %>%
  left_join(prenatal_v, by = "CASEID") %>%
  mutate(
    HHID = as.numeric(str_sub(CASEID,1,-3)),
    year = 2020
  ) %>% 
  left_join(nacion_v %>% mutate(HHID = as.numeric(HHID)),by = c("HHID")) %>% 
  left_join(seguro_v %>% mutate(HHID = as.numeric(HHID)),by = c("HHID"))

dim(df_gestantes2020)

############################################ FALTA : COMO 2020 #####################################
## Gestantes 2019

gestacion1 <- read_sav("./data/raw/endes back up/2019/RE223132.sav")
gestacion2 <- read_sav("./data/raw/endes back up/2019/REC94.sav") # outcome # id
violence <- read_sav("./data/raw/endes back up/2019/REC84DV.sav")
sociodemo <- read_sav("./data/raw/endes back up/2019/REC0111.sav")
vih1 <- read_sav("./data/raw/endes back up/2019/RE758081.sav")
vih2 <- read_sav("./data/raw/endes back up/2019/REC91.sav")
# Gestantes
df_gestantes2019<-
  gestacion2 %>% 
  left_join(gestacion1, by = "CASEID") %>% 
  left_join(violence, by = "CASEID") %>% 
  left_join(sociodemo, by = "CASEID") %>%
  left_join(vih1, by = "CASEID") %>% 
  left_join(vih2, by = "CASEID") %>%
  mutate(year = "2019")


## Gestantes 2018

gestacion1 <- read_sav("./data/raw/endes back up/2018/RE223132.sav")
gestacion2 <- read_sav("./data/raw/endes back up/2018/REC94.sav") # outcome # id
violence <- read_sav("./data/raw/endes back up/2018/REC84DV.sav")
sociodemo <- read_sav("./data/raw/endes back up/2018/REC0111.sav")
vih1 <- read_sav("./data/raw/endes back up/2018/RE758081.sav")
vih2 <- read_sav("./data/raw/endes back up/2018/REC91.sav")
# Gestantes
df_gestantes2018<-
  gestacion2 %>% 
  left_join(gestacion1, by = "CASEID") %>% 
  left_join(violence, by = "CASEID") %>% 
  left_join(sociodemo, by = "CASEID") %>%
  left_join(vih1, by = "CASEID") %>% 
  left_join(vih2, by = "CASEID") %>%
  mutate(year = "2018")



## Gestantes 2017

gestacion1 <- read_sav("./data/raw/endes back up/2017/RE223132.sav")
gestacion2 <- read_sav("./data/raw/endes back up/2017/REC94.sav") # outcome # id
violence <- read_sav("./data/raw/endes back up/2017/REC84DV.sav")
sociodemo <- read_sav("./data/raw/endes back up/2017/REC0111.sav")
vih1 <- read_sav("./data/raw/endes back up/2017/RE758081.sav")
vih2 <- read_sav("./data/raw/endes back up/2017/REC91.sav")
# Gestantes
df_gestantes2017<-
  gestacion2 %>% 
  left_join(gestacion1, by = "CASEID") %>% 
  left_join(violence, by = "CASEID") %>% 
  left_join(sociodemo, by = "CASEID") %>%
  left_join(vih1, by = "CASEID") %>% 
  left_join(vih2, by = "CASEID") %>%
  mutate(year = "2017")


## Gestantes 2016

gestacion1 <- read_sav("./data/raw/endes back up/2016/RE223132.sav")
gestacion2 <- read_sav("./data/raw/endes back up/2016/REC94.sav") # outcome # id
violence <- read_sav("./data/raw/endes back up/2016/REC84DV.sav")
sociodemo <- read_sav("./data/raw/endes back up/2016/REC0111.sav")
vih1 <- read_sav("./data/raw/endes back up/2016/RE758081.sav")
vih2 <- read_sav("./data/raw/endes back up/2016/REC91.sav")
# Gestantes
df_gestantes2016<-
  gestacion2 %>% 
  left_join(gestacion1, by = "CASEID") %>% 
  left_join(violence, by = "CASEID") %>% 
  left_join(sociodemo, by = "CASEID") %>%
  left_join(vih1, by = "CASEID") %>% 
  left_join(vih2, by = "CASEID") %>%
  mutate(year = "2016",
         V005 = v005)




## Gestantes 2015

gestacion1 <- read_sav("./data/raw/endes back up/2015/RE223132.sav")
gestacion2 <- read_sav("./data/raw/endes back up/2015/REC94.sav") # outcome # id
violence <- read_sav("./data/raw/endes back up/2015/REC84DV.sav")
sociodemo <- read_sav("./data/raw/endes back up/2015/REC0111.sav")
vih1 <- read_sav("./data/raw/endes back up/2015/RE758081.sav")
vih2 <- read_sav("./data/raw/endes back up/2015/REC91.sav")
# Gestantes
df_gestantes2015<-
  gestacion2 %>% 
  left_join(gestacion1, by = "CASEID") %>% 
  left_join(violence, by = "CASEID") %>% 
  left_join(sociodemo, by = "CASEID") %>%
  left_join(vih1, by = "CASEID") %>% 
  left_join(vih2, by = "CASEID") %>%
  mutate(year = "2015")


## Gestantes 2014

gestacion1 <- read_sav("./data/raw/endes back up/2014/RE223132.sav")
gestacion2 <- read_sav("./data/raw/endes back up/2014/REC94.sav") # outcome # id
violence <- read_sav("./data/raw/endes back up/2014/REC84DV.sav")
sociodemo <- read_sav("./data/raw/endes back up/2014/REC0111.sav")
vih1 <- read_sav("./data/raw/endes back up/2014/RE758081.sav")
vih2 <- read_sav("./data/raw/endes back up/2014/REC91.sav")
# Gestantes
df_gestantes2014<-
  gestacion2 %>% 
  left_join(gestacion1, by = "CASEID") %>% 
  left_join(violence, by = "CASEID") %>% 
  left_join(sociodemo, by = "CASEID") %>%
  left_join(vih1, by = "CASEID") %>% 
  left_join(vih2, by = "CASEID") %>%
  mutate(year = "2014")


## Gestantes 2013

gestacion1 <- read_sav("./data/raw/endes back up/2013/RE223132.sav") %>% select(-(V001:V191)) # variables se repiten en encuesta REC0011 y generan conflicto al unirlas
gestacion2 <- read_sav("./data/raw/endes back up/2013/REC94.sav") # outcome # id
violence <- read_sav("./data/raw/endes back up/2013/REC84DV.sav")
sociodemo <- read_sav("./data/raw/endes back up/2013/REC0111.sav")
vih1 <- read_sav("./data/raw/endes back up/2013/RE758081.sav")
vih2 <- read_sav("./data/raw/endes back up/2013/REC91.sav")
# Gestantes
df_gestantes2013<-
  gestacion2 %>% 
  left_join(gestacion1, by = "CASEID") %>% 
  left_join(violence, by = "CASEID") %>% 
  left_join(sociodemo, by = "CASEID") %>%
  left_join(vih1, by = "CASEID") %>% 
  left_join(vih2, by = "CASEID") %>%
  mutate(year = "2013")


## Gestantes 2012

gestacion1 <- read_sav("./data/raw/endes back up/2012/RE212232.sav")
gestacion2 <- read_sav("./data/raw/endes back up/2012/REC94.sav") # outcome # id
violence <- read_sav("./data/raw/endes back up/2012/REC84DV.sav")
sociodemo <- read_sav("./data/raw/endes back up/2012/REC0111.sav")
vih1 <- read_sav("./data/raw/endes back up/2012/RE758081.sav")
vih2 <- read_sav("./data/raw/endes back up/2012/REC91.sav")
# Gestantes
df_gestantes2012<-
  gestacion2 %>% 
  left_join(gestacion1, by = "CASEID") %>% 
  left_join(violence, by = "CASEID") %>% 
  left_join(sociodemo, by = "CASEID") %>%
  left_join(vih1, by = "CASEID") %>% 
  left_join(vih2, by = "CASEID") %>%
  mutate(year = "2012")


## Gestantes 2011

gestacion1 <- read_sav("./data/raw/endes back up/2011/RE223132.sav")
gestacion2 <- read_sav("./data/raw/endes back up/2011/REC94.sav") # outcome # id
violence <- read_sav("./data/raw/endes back up/2011/REC84DV.sav")
sociodemo <- read_sav("./data/raw/endes back up/2011/REC0111.sav")
vih1 <- read_sav("./data/raw/endes back up/2011/RE758081.sav")
vih2 <- read_sav("./data/raw/endes back up/2011/REC91.sav")
# Gestantes
df_gestantes2011<-
  gestacion2 %>% 
  left_join(gestacion1, by = "CASEID") %>% 
  left_join(violence, by = "CASEID") %>% 
  left_join(sociodemo, by = "CASEID") %>%
  left_join(vih1, by = "CASEID") %>% 
  left_join(vih2, by = "CASEID") %>%
  mutate(year = "2011")


## Gestantes 2010

gestacion1 <- read_sav("./data/raw/endes back up/2010/RE223132.sav")
gestacion2 <- read_sav("./data/raw/endes back up/2010/REC94.sav") # outcome # id
violence <- read_sav("./data/raw/endes back up/2010/REC84DV.sav")
sociodemo <- read_sav("./data/raw/endes back up/2010/REC0111.sav")
vih1 <- read_sav("./data/raw/endes back up/2010/RE758081.sav")
vih2 <- read_sav("./data/raw/endes back up/2010/REC91.sav")
# Gestantes
df_gestantes2010<-
  gestacion2 %>% 
  left_join(gestacion1, by = "CASEID") %>% 
  left_join(violence, by = "CASEID") %>% 
  left_join(sociodemo, by = "CASEID") %>%
  left_join(vih1, by = "CASEID") %>% 
  left_join(vih2, by = "CASEID") %>%
  mutate(year = "2010")


# Union de bases

#Hasta aqui ya esta cada base por cada anio generada. Lo que sigue es unir las bases (cada una tiene una variable "year" que indica el anio). Lo que he hecho es unir de dos en dos y luego agruparlas. Deberia poder hacerse defrente de las 6 con bind_rows pero no se porque no me funciona. Lo revisare luego. Igual, posiblemente hagamos un paso extra mas si creamos una funcion para seleccionar/crear las variables que vayamos a utilizar para el analisis. Cuando nos reunamos les ensenio que hice con el proyecto de vacunas. 

list<- list(df_gestantes2010,df_gestantes2011,df_gestantes2012,
            df_gestantes2013,df_gestantes2014,df_gestantes2015,
            df_gestantes2016,df_gestantes2017,df_gestantes2018,
            df_gestantes2019,df_gestantes2020)
dataset<- map_df(.x = list,
                 .f = ~dataprep(.x)) # funcion de prueba creada
write.csv(dataset,"./data/data_analysis.csv", row.names = F)
rm(list=ls()) # Elimina todos los objetos del enviroment


install.packages("gtsummary")

library(here)
library(tidyverse)
library(haven)
library(gtsummary)
library(survey)
library(RColorBrewer)
library(sf)

violence <- read_sav("2020/REC84DV.sav")
sociodemo <- read_sav("2020/REC0111.sav")
vih1 <- read_sav("2020/RE758081.sav")
vih2 <- read_sav("2020/REC91.sav")
gestacion1 <- read_sav("2020/RE223132.sav")
gestacion2 <- read_sav("2020/REC94.sav")
programas <- read_sav("2020/Programas Sociales x Hogar.sav")

violence <- violence %>% select(CASEID,D101A:D101F,D104,D106,D107,D108) %>%
  mutate(HHID = as.numeric(str_sub(CASEID,1,-3)))

sociodemo <-

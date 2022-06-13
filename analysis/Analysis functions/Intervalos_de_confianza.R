library(tidyverse)
library(survey)
library(dplyr)
data_read<-read.csv("./data/data2021final.csv")
attach(data_read)
library(naniar)


data2 <- data_read %>% mutate (EDU_LEVEL = recode(EDU_LEVEL, 'NONE/PRESCHOOL' = 'PRIMARY')) %>%
  mutate(ETHNICITY = recode(ETHNICITY, 'AIMARA' = 'QUECHUA')) %>% 
  mutate(ETHNICITY = recode(ETHNICITY, 'OTHER INDIGENOUS' = 'QUECHUA')) %>% 
  mutate(across(where(is.character), ~na_if(., "FOREIGNER"))) %>% 
  filter(year %in% c('2010','2015','2019','2021'))
  
head(data2)

length(CASEID)
length(unique(V001))

data3 <- data2 %>% select(-CASEID)

#----------------------------------------
# Transformando la data en objeto survey
#----------------------------------------

df<-
  data3 %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    datasvy = map(.x = data,
                  .f = ~svydesign(id =~ V001, strata =~ V022, weights=~V005, data=.x))
  )
options(survey.lonely.psu="remove")

df
head(df$data)

#AGE MOTHER
df2 <- df %>% mutate(
  agemother_ci = map(.x = datasvy,
                       .f = ~svymean(~as.factor(AGE_MOTHER), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                         confint() %>% 
                         as.data.frame() %>% 
                         rownames_to_column(var = "var")))


df2$agemother_ci


#EDUCATION LEVEL
df3 <- df %>% mutate(
  edulevel_ci = map(.x = datasvy,
                     .f = ~svymean(~as.factor(EDU_LEVEL), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                       confint() %>% 
                       as.data.frame() %>% 
                       rownames_to_column(var = "var")))


df3$edulevel_ci

#MARITAL STATUS
df4 <- df %>% mutate(
  maritalstatus_ci = map(.x = datasvy,
                    .f = ~svymean(~as.factor(CURRENT_MARITAL_STATUS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                      confint() %>% 
                      as.data.frame() %>% 
                      rownames_to_column(var = "var")))


df4$maritalstatus_ci

#NATIVE LANGUAGE: ETHNICITY
df4 <- df %>% mutate(
  native_language_ci = map(.x = datasvy,
                         .f = ~svymean(~as.factor(ETHNICITY), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                           confint() %>% 
                           as.data.frame() %>% 
                           rownames_to_column(var = "var")))


df4$native_language_ci

#KNOWS STD: KNOWS ETS
df4 <- df %>% mutate(
  knowets_ci = map(.x = datasvy,
                           .f = ~svymean(~as.factor(KNOW_ETS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                             confint() %>% 
                             as.data.frame() %>% 
                             rownames_to_column(var = "var")))


df4$knowets_ci

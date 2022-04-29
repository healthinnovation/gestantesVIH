library(tidyverse)
library(survey)
data_read<-read.csv("./data/datafinal.csv")
attach(data_read)

data2 <- data_read %>% filter(year %in% c('2010','2015','2019','2020'))
head(data2)

length(ID)
length(unique(V001))

data3 <- data2 %>% select(-ID)

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

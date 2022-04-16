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

df2 <- df %>% mutate(
  agemother_ci = map(.x = datasvy,
                       .f = ~svymean(~as.factor(AGE_MOTHER), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                         confint() %>% 
                         as.data.frame() %>% 
                         rownames_to_column(var = "var")))


df2$agemother_ci

"
data = data %>% mutate (INSTITU = recode_factor(PRENATAL_ATTENTION_PLACE,FF.AA. = 'OTHERS'))

table(data$INSTITU)
table(data$INSTITU,data$year)

data %>% group_by(year) %>% summarise(MEDIANA = median(FIRST_PRENATAL_VISIT, na.rm = T))

data %>% group_by(year) %>% summarise(MEDIANA = median(NUMBER_PRENATAL_VISITS, na.rm = T))

table(data$HOUSEHOLD_MEMBERS, data$year)
"

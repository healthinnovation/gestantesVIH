library(tidymodels)
library(tidyverse)

data_read <- read.csv("./data/datatest.csv")
glimpse(data_read)

data <- data_read %>% select(-c(CASEID,V001,V022, V005, IDX94, B2, B3, filtro))
glimpse(data)

table(data$year)*100/nrow(data)
table(data$CHECKUP_RULE_OUT_HIV)*100/nrow(data)

sum(is.na(data$CHECKUP_RULE_OUT_HIV)) *100/nrow(data)

df <- data %>% filter(year != ('2019') & !is.na(CHECKUP_RULE_OUT_HIV))

table(df$year)
sum(is.na(df$CHECKUP_RULE_OUT_HIV)) *100/nrow(df)
sum(is.na(df$CHECKUP_RULE_OUT_SYPHILIS)) *100/nrow(df)

#----- Valores Nulos
round(colSums(is.na(df))*100/nrow(df),2)
names(df[colSums(is.na(df))*100/nrow(df)>0])


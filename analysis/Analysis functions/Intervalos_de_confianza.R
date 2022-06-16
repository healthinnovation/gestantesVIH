library(tidyverse)
library(survey)
library(dplyr)
data_read<-read.csv("./data/data2021final.csv")
attach(data_read)



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
df5 <- df %>% mutate(
  knowets_ci = map(.x = datasvy,
                           .f = ~svymean(~as.factor(KNOW_ETS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                             confint() %>% 
                             as.data.frame() %>% 
                             rownames_to_column(var = "var")))


df5$knowets_ci

#KNOW STD SYMPTOMS: KNOW_SYMPTON_ETS

df6 <- df %>% mutate(
  knowets_symptoms_ci = map(.x = datasvy,
                   .f = ~svymean(~as.factor(KNOW_SYMPTON_ETS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                     confint() %>% 
                     as.data.frame() %>% 
                     rownames_to_column(var = "var")))


df6$knowets_symptoms_ci

#SYMPTOMS RELATED TO STD: HAVE_ITS_SYMPTOMS

df7 <- df %>% mutate(
  have_its_symptoms_ci = map(.x = datasvy,
                            .f = ~svymean(~as.factor(HAVE_ITS_SYMPTOMS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                              confint() %>% 
                              as.data.frame() %>% 
                              rownames_to_column(var = "var")))


df7$have_its_symptoms_ci

#KNOW VERTICAL TRANSMISSION: KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD

df8 <- df %>% mutate(
  know_vertical_transmission_ci = map(.x = datasvy,
                             .f = ~svymean(~as.factor(KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                               confint() %>% 
                               as.data.frame() %>% 
                               rownames_to_column(var = "var")))


df8$know_vertical_transmission_ci

# INTENDED PREGNANCY: INTENDED_PREGNANCY

df9 <- df %>% mutate(
  intended_pregnancy_ci = map(.x = datasvy,
                                      .f = ~svymean(~as.factor(INTENDED_PREGNANCY), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                                        confint() %>% 
                                        as.data.frame() %>% 
                                        rownames_to_column(var = "var")))


df9$intended_pregnancy_ci

# PHYSICAL_VIOLENCE: PHYSICAL_VIOLENCE

df10 <- df %>% mutate(
  physical_violence_ci = map(.x = datasvy,
                              .f = ~svymean(~as.factor(PHYSICAL_VIOLENCE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                                confint() %>% 
                                as.data.frame() %>% 
                                rownames_to_column(var = "var")))


df10$physical_violence_ci


#SYPHILIS: CHECKUP_RULE_OUT_SYPHILIS

df11 <- df %>% mutate(
  syphilis_ci = map(.x = datasvy,
                             .f = ~svymean(~as.factor(CHECKUP_RULE_OUT_SYPHILIS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                               confint() %>% 
                               as.data.frame() %>% 
                               rownames_to_column(var = "var")))


df11$syphilis_ci

#HIV: CHECKUP_RULE_OUT_HIV

df12 <- df %>% mutate(
  hiv_ci = map(.x = datasvy,
                    .f = ~svymean(~as.factor(CHECKUP_RULE_OUT_HIV), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                      confint() %>% 
                      as.data.frame() %>% 
                      rownames_to_column(var = "var")))


df12$hiv_ci

# RESIDENCE: TYPE_PLACE_RESIDENCE

df13 <- df %>% mutate(
  residence_ci = map(.x = datasvy,
               .f = ~svymean(~as.factor(TYPE_PLACE_RESIDENCE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                 confint() %>% 
                 as.data.frame() %>% 
                 rownames_to_column(var = "var")))


df13$residence_ci

# NATURAL REGION: NATURAL_REGION

df14 <- df %>% mutate(
  natural_region_ci = map(.x = datasvy,
                     .f = ~svymean(~as.factor(NATURAL_REGION), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                       confint() %>% 
                       as.data.frame() %>% 
                       rownames_to_column(var = "var")))


df14$natural_region_ci

# WEALTH INDEX: WEALTH_INDEX

df15 <- df %>% mutate(
  wealth_index_ci = map(.x = datasvy,
                          .f = ~svymean(~as.factor(WEALTH_INDEX), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                            confint() %>% 
                            as.data.frame() %>% 
                            rownames_to_column(var = "var")))


df15$wealth_index_ci

# RELATIONSHIP WITH THE HOUSEHOLD HEAD: RELATIONSHIP_HOUSEHOLD_HEAD

df16 <- df %>% mutate(
  house_head_ci = map(.x = datasvy,
                        .f = ~svymean(~as.factor(RELATIONSHIP_HOUSEHOLD_HEAD), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                          confint() %>% 
                          as.data.frame() %>% 
                          rownames_to_column(var = "var")))


df16$house_head_ci

# NUMBER OF MEMBERS: HOUSEHOLD_MEMBERS

df17 <- df %>% mutate(
  house_members_ci = map(.x = datasvy,
                         .f = ~svymean(~as.factor(HOUSEHOLD_MEMBERS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                           confint() %>% 
                           as.data.frame() %>% 
                           rownames_to_column(var = "var")))


df17$house_members_ci

# INSURANCE: HEALTH_INSURANCE

df18 <- df %>% mutate(
  insurance_ci = map(.x = datasvy,
                         .f = ~svymean(~as.factor(HEALTH_INSURANCE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                           confint() %>% 
                           as.data.frame() %>% 
                           rownames_to_column(var = "var")))


df18$insurance_ci

# COMPLEXITY CENTER: COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE

df19 <- df %>% mutate(
  complexity_ci = map(.x = datasvy,
                     .f = ~svymean(~as.factor(COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                       confint() %>% 
                       as.data.frame() %>% 
                       rownames_to_column(var = "var")))


df19$complexity_ci

# INSTITUTION: PRENATAL_ATTENTION_PLACE

df20 <- df %>% mutate(
  institution_ci = map(.x = datasvy,
                      .f = ~svymean(~as.factor(PRENATAL_ATTENTION_PLACE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                        confint() %>% 
                        as.data.frame() %>% 
                        rownames_to_column(var = "var")))


df20$institution_ci


# FIRST PRENATAL VISIT: FIRST_PRENATAL_VISIT

df21 <- df %>% mutate(
  first_visit_ci = map(.x = datasvy,
                       .f = ~svymean(~as.factor(FIRST_PRENATAL_VISIT), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                         confint() %>% 
                         as.data.frame() %>% 
                         rownames_to_column(var = "var")))


df21$first_visit_ci

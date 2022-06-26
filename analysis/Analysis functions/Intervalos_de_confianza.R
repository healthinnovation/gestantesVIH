library(tidyverse)
library(survey)
library(dplyr)
library(gtsummary)
data_read<-read.csv("./data/data2021final.csv")
attach(data_read)

install.packages("tmap")
library(tmap)


data2 <- data_read %>% mutate (EDU_LEVEL = recode(EDU_LEVEL, 'NONE/PRESCHOOL' = 'PRIMARY')) %>%
  mutate(ETHNICITY = recode(ETHNICITY, 'AIMARA' = 'QUECHUA')) %>% 
  mutate(ETHNICITY = recode(ETHNICITY, 'OTHER INDIGENOUS' = 'QUECHUA')) %>% 
  mutate(across(where(is.character), ~na_if(., "FOREIGNER"))) %>% 
  mutate(PRENATAL_ATTENTION_PLACE = recode(PRENATAL_ATTENTION_PLACE, 'FF.AA.' = 'OTHERS')) %>% 
  mutate(PRENATAL_ATTENTION_PLACE = recode(PRENATAL_ATTENTION_PLACE, 'PRIVATE' = 'OTHERS')) %>%
  filter(year %in% c ('2010', '2015', '2019', '2021'))
  
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

#VARIABLES AT INDIVIDUAL LEVEL
df2 <- df %>% 
  mutate(
    
    agemother_m = map(.x = datasvy,
                        .f = ~svymean(~as.factor(AGE_MOTHER), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                          as.vector() %>% 
                          as.data.frame() %>% 
                          mutate("prop" = as.numeric(.))%>% 
                          select(-.)),
    
    agemother_ci = map(.x = datasvy,
                       .f = ~svymean(~as.factor(AGE_MOTHER), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                         confint() %>% 
                         as.data.frame() %>% 
                         rownames_to_column(var = "var")),
    
    agemother = map2(.x = agemother_m,
                            .y = agemother_ci,
                             .f = ~bind_cols(.x,.y)),  

    edulevel_ci = map(.x = datasvy,
                     .f = ~svymean(~as.factor(EDU_LEVEL), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                       confint() %>% 
                       as.data.frame() %>% 
                       rownames_to_column(var = "var")),
    
    edulevel_m = map(.x = datasvy,
                     .f = ~svymean(~as.factor(EDU_LEVEL), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                      as.vector() %>% 
                      as.data.frame() %>% 
                      mutate("prop" = as.numeric(.))%>% 
                      select(-.)),
    
    edulevel = map2(.x = edulevel_m,
                     .y = edulevel_ci,
                     .f = ~bind_cols(.x,.y)),  
    
    maritalstatus_ci = map(.x = datasvy,
                    .f = ~svymean(~as.factor(CURRENT_MARITAL_STATUS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                      confint() %>% 
                      as.data.frame() %>% 
                      rownames_to_column(var = "var")),

    maritalstatus_m = map(.x = datasvy,
                   .f = ~svymean(~as.factor(CURRENT_MARITAL_STATUS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                     as.vector() %>% 
                     as.data.frame() %>% 
                     mutate("prop" = as.numeric(.))%>% 
                     select(-.)),
    
    maritalstatus = map2(.x = maritalstatus_m,
                    .y = maritalstatus_ci,
                    .f = ~bind_cols(.x,.y)),  

    native_language_ci = map(.x = datasvy,
                         .f = ~svymean(~as.factor(ETHNICITY), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                           confint() %>% 
                           as.data.frame() %>% 
                           rownames_to_column(var = "var")),


    native_language_m = map(.x = datasvy,
                        .f = ~svymean(~as.factor(ETHNICITY), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                          as.vector() %>% 
                          as.data.frame() %>% 
                          mutate("prop" = as.numeric(.))%>% 
                          select(-.)),

    native_language = map2(.x = native_language_m,
                         .y = native_language_ci,
                         .f = ~bind_cols(.x,.y)),  
    

    knowets_ci = map(.x = datasvy,
                           .f = ~svymean(~as.factor(KNOW_ETS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                             confint() %>% 
                             as.data.frame() %>% 
                             rownames_to_column(var = "var")),
    
    knowets_m = map(.x = datasvy,
                            .f = ~svymean(~as.factor(KNOW_ETS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                              as.vector() %>% 
                              as.data.frame() %>% 
                              mutate("prop" = as.numeric(.))%>% 
                              select(-.)),
  
    knowets = map2(.x = knowets_m,
                           .y = knowets_ci,
                           .f = ~bind_cols(.x,.y)),  
    
    knowets_symptoms_ci = map(.x = datasvy,
                   .f = ~svymean(~as.factor(KNOW_SYMPTON_ETS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                     confint() %>% 
                     as.data.frame() %>% 
                     rownames_to_column(var = "var")),
   
   
   knowets_symptoms_m = map(.x = datasvy,
                   .f = ~svymean(~as.factor(KNOW_SYMPTON_ETS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                     as.vector() %>% 
                     as.data.frame() %>% 
                     mutate("prop" = as.numeric(.))%>% 
                     select(-.)),

   knowets_symptoms = map2(.x = knowets_symptoms_m,
                  .y = knowets_symptoms_ci,
                  .f = ~bind_cols(.x,.y)),  
   
   have_its_symptoms_ci = map(.x = datasvy,
                            .f = ~svymean(~as.factor(HAVE_ITS_SYMPTOMS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                              confint() %>% 
                              as.data.frame() %>% 
                              rownames_to_column(var = "var")),
  
   have_its_symptoms_m = map(.x = datasvy,
                            .f = ~svymean(~as.factor(HAVE_ITS_SYMPTOMS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                              as.vector() %>% 
                              as.data.frame() %>% 
                              mutate("prop" = as.numeric(.))%>% 
                              select(-.)),
   
   have_its_symptoms = map2(.x = have_its_symptoms_m,
                           .y = have_its_symptoms_ci,
                           .f = ~bind_cols(.x,.y)),  
   
   know_vertical_transmission_ci = map(.x = datasvy,
                             .f = ~svymean(~as.factor(KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                               confint() %>% 
                               as.data.frame() %>% 
                               rownames_to_column(var = "var")),
   
    know_vertical_transmission_m = map(.x = datasvy,
                             .f = ~svymean(~as.factor(KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                               as.vector() %>% 
                               as.data.frame() %>% 
                               mutate("prop" = as.numeric(.))%>% 
                               select(-.)),
   
    know_vertical_transmission = map2(.x = know_vertical_transmission_m,
                            .y = know_vertical_transmission_ci,
                            .f = ~bind_cols(.x,.y)),  

   intended_pregnancy_ci = map(.x = datasvy,
                                      .f = ~svymean(~as.factor(INTENDED_PREGNANCY), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                                        confint() %>% 
                                        as.data.frame() %>% 
                                        rownames_to_column(var = "var")),
   
   intended_pregnancy_m = map(.x = datasvy,
                                      .f = ~svymean(~as.factor(INTENDED_PREGNANCY), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                                        as.vector() %>% 
                                        as.data.frame() %>% 
                                        mutate("prop" = as.numeric(.))%>% 
                                        select(-.)),
   
   intended_pregnancy = map2(.x = intended_pregnancy_m,
                                     .y = intended_pregnancy_ci,
                                     .f = ~bind_cols(.x,.y)),  

   physical_violence_ci = map(.x = datasvy,
                              .f = ~svymean(~as.factor(PHYSICAL_VIOLENCE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                                confint() %>% 
                                as.data.frame() %>% 
                                rownames_to_column(var = "var")),
   
   physical_violence_m = map(.x = datasvy,
                              .f = ~svymean(~as.factor(PHYSICAL_VIOLENCE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                                as.vector() %>% 
                                as.data.frame() %>% 
                                mutate("prop" = as.numeric(.))%>% 
                                select(-.)),

   physical_violence = map2(.x = physical_violence_m,
                            .y = physical_violence_ci,
                            .f = ~bind_cols(.x,.y)),  

   syphilis_ci = map(.x = datasvy,
                             .f = ~svymean(~as.factor(CHECKUP_RULE_OUT_SYPHILIS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                               confint() %>% 
                               as.data.frame() %>% 
                               rownames_to_column(var = "var")),
   
   syphilis_m = map(.x = datasvy,
                             .f = ~svymean(~as.factor(CHECKUP_RULE_OUT_SYPHILIS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                               as.vector() %>% 
                               as.data.frame() %>% 
                               mutate("prop" = as.numeric(.))%>% 
                               select(-.)),

   syphilis = map2(.x = syphilis_m,
                            .y = syphilis_ci,
                            .f = ~bind_cols(.x,.y)),  
   
   hiv_ci = map(.x = datasvy,
                    .f = ~svymean(~as.factor(CHECKUP_RULE_OUT_HIV), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                      confint() %>% 
                      as.data.frame() %>% 
                      rownames_to_column(var = "var")),
   
   hiv_m = map(.x = datasvy,
                    .f = ~svymean(~as.factor(CHECKUP_RULE_OUT_HIV), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                      as.vector() %>% 
                      as.data.frame() %>% 
                      mutate("prop" = as.numeric(.))%>% 
                      select(-.)),
   
   hiv = map2(.x = hiv_m,
                   .y = hiv_ci,
                   .f = ~bind_cols(.x,.y)))  
   

##Creando las tablas


   age_mother<-
     df2 %>% 
     select(year,agemother) %>% 
     unnest(cols = c("agemother")) %>% mutate(prop = round(prop*100, digits = 1),
                                                    `2.5 %`=round(`2.5 %`*100, digits = 1),
                                                    `97.5 %`=round(`97.5 %`*100, digits = 1),
                                                    variable = "age_mother")
   
   
   edulevel_mother<-
     df2 %>% 
     select(year,edulevel) %>% 
     unnest(cols = c("edulevel")) %>% mutate(prop = round(prop*100, digits = 1),
                                              `2.5 %`=round(`2.5 %`*100, digits = 1),
                                              `97.5 %`=round(`97.5 %`*100, digits = 1),
                                              variable = "edulevel_mother")
   
   
   
   maritalstatus<-
     df2 %>% 
     select(year,maritalstatus) %>% 
     unnest(cols = c("maritalstatus")) %>% mutate(prop = round(prop*100, digits = 1),
                                             `2.5 %`=round(`2.5 %`*100, digits = 1),
                                             `97.5 %`=round(`97.5 %`*100, digits = 1),
                                             variable = "maritalstatus")
   
   native_language<-
     df2 %>% 
     select(year,native_language) %>% 
     unnest(cols = c("native_language")) %>% mutate(prop = round(prop*100, digits = 1),
                                                  `2.5 %`=round(`2.5 %`*100, digits = 1),
                                                  `97.5 %`=round(`97.5 %`*100, digits = 1),
                                                  variable = "native_language")
   
   knowets<-
     df2 %>% 
     select(year,knowets) %>% 
     unnest(cols = c("knowets")) %>% mutate(prop = round(prop*100, digits = 1),
                                                    `2.5 %`=round(`2.5 %`*100, digits = 1),
                                                    `97.5 %`=round(`97.5 %`*100, digits = 1),
                                                    variable = "knowets")
   knowets_symptoms<-
     df2 %>% 
     select(year,knowets_symptoms) %>% 
     unnest(cols = c("knowets_symptoms")) %>% mutate(prop = round(prop*100, digits = 1),
                                            `2.5 %`=round(`2.5 %`*100, digits = 1),
                                            `97.5 %`=round(`97.5 %`*100, digits = 1),
                                            variable = "knowets_symptoms")
   
   have_its_symptoms<-
     df2 %>% 
     select(year,have_its_symptoms) %>% 
     unnest(cols = c("have_its_symptoms")) %>% mutate(prop = round(prop*100, digits = 1),
                                                     `2.5 %`=round(`2.5 %`*100, digits = 1),
                                                     `97.5 %`=round(`97.5 %`*100, digits = 1),
                                                     variable = "have_its_symptoms")
     
   know_vertical_transmission<-
     df2 %>% 
     select(year,know_vertical_transmission) %>% 
     unnest(cols = c("know_vertical_transmission")) %>% mutate(prop = round(prop*100, digits = 1),
                                                      `2.5 %`=round(`2.5 %`*100, digits = 1),
                                                      `97.5 %`=round(`97.5 %`*100, digits = 1),
                                                      variable = "know_vertical_transmission")
   
   intended_pregnancy<-
     df2 %>% 
     select(year,intended_pregnancy) %>% 
     unnest(cols = c("intended_pregnancy")) %>% mutate(prop = round(prop*100, digits = 1),
                                                               `2.5 %`=round(`2.5 %`*100, digits = 1),
                                                               `97.5 %`=round(`97.5 %`*100, digits = 1),
                                                               variable = "intended_pregnancy")
   
   physical_violence<-
     df2 %>% 
     select(year,physical_violence) %>% 
     unnest(cols = c("physical_violence")) %>% mutate(prop = round(prop*100, digits = 1),
                                                       `2.5 %`=round(`2.5 %`*100, digits = 1),
                                                       `97.5 %`=round(`97.5 %`*100, digits = 1),
                                                       variable = "physical_violence")
   
   syphilis <-
     df2 %>% 
     select(year,syphilis) %>% 
     unnest(cols = c("syphilis")) %>% mutate(prop = round(prop*100, digits = 1),
                                                      `2.5 %`=round(`2.5 %`*100, digits = 1),
                                                      `97.5 %`=round(`97.5 %`*100, digits = 1),
                                                      variable = "syphilis")
   
   hiv<-
     df2 %>% 
     select(year,hiv) %>% 
     unnest(cols = c("hiv")) %>% mutate(prop = round(prop*100, digits = 1),
                                             `2.5 %`=round(`2.5 %`*100, digits = 1),
                                             `97.5 %`=round(`97.5 %`*100, digits = 1),
                                             variable = "hiv")
   
   descriptivo<-
     bind_rows(age_mother, edulevel_mother, maritalstatus, native_language, knowets, knowets_symptoms, have_its_symptoms, 
               know_vertical_transmission, intended_pregnancy, physical_violence, syphilis, hiv) %>% 
     select(year,var,prop,`2.5 %`,`97.5 %`,variable)

   
   write.csv(descriptivo,"./data/descriptivo_individual.csv", row.names = F)
   
# VARIABLES AT HOUSEHOLD LEVEL

   df3 <- df %>% 
     mutate(
   residence_ci = map(.x = datasvy,
                .f = ~svymean(~as.factor(TYPE_PLACE_RESIDENCE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                  confint() %>% 
                  as.data.frame() %>% 
                  rownames_to_column(var = "var")),
   
   residence_m = map(.x = datasvy,
               .f = ~svymean(~as.factor(TYPE_PLACE_RESIDENCE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                 as.vector() %>% 
                 as.data.frame() %>% 
                 mutate("prop" = as.numeric(.))%>% 
                 select(-.)),
   
   residence = map2(.x = residence_m,
              .y = residence_ci,
              .f = ~bind_cols(.x,.y)),
   
   natural_region_ci = map(.x = datasvy,
                           .f = ~svymean(~as.factor(NATURAL_REGION), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                             confint() %>% 
                             as.data.frame() %>% 
                             rownames_to_column(var = "var")),
   
   natural_region_m = map(.x = datasvy,
                          .f = ~svymean(~as.factor(NATURAL_REGION), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                            as.vector() %>% 
                            as.data.frame() %>% 
                            mutate("prop" = as.numeric(.))%>% 
                            select(-.)),

   natural_region = map2(.x = natural_region_m,
                    .y = natural_region_ci,
                    .f = ~bind_cols(.x,.y)),
   

  wealth_index_ci = map(.x = datasvy,
                          .f = ~svymean(~as.factor(WEALTH_INDEX), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                            confint() %>% 
                            as.data.frame() %>% 
                            rownames_to_column(var = "var")),
  
  wealth_index_m = map(.x = datasvy,
                         .f = ~svymean(~as.factor(WEALTH_INDEX), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                           as.vector() %>% 
                           as.data.frame() %>% 
                           mutate("prop" = as.numeric(.))%>% 
                           select(-.)),

  wealth_index = map2(.x = wealth_index_m,
                        .y = wealth_index_ci,
                        .f = ~bind_cols(.x,.y)),
  

  house_head_ci = map(.x = datasvy,
                         .f = ~svymean(~as.factor(RELATIONSHIP_HOUSEHOLD_HEAD), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                           confint() %>% 
                           as.data.frame() %>% 
                           rownames_to_column(var = "var")),

   
  house_head_m = map(.x = datasvy,
                        .f = ~svymean(~as.factor(RELATIONSHIP_HOUSEHOLD_HEAD), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                          as.vector() %>% 
                          as.data.frame() %>% 
                          mutate("prop" = as.numeric(.))%>% 
                          select(-.)),
   
  house_head = map2(.x = house_head_m,
                       .y = house_head_ci,
                       .f = ~bind_cols(.x,.y)),

   house_members_ci = map(.x = datasvy,
                          .f = ~svymean(~as.factor(HOUSEHOLD_MEMBERS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                            confint() %>% 
                            as.data.frame() %>% 
                            rownames_to_column(var = "var")),
   
   
   house_members_m = map(.x = datasvy,
                         .f = ~svymean(~as.factor(HOUSEHOLD_MEMBERS), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                           as.vector() %>% 
                           as.data.frame() %>% 
                           mutate("prop" = as.numeric(.))%>% 
                           select(-.)),
   
   house_members = map2(.x = house_members_m,
                        .y = house_members_ci,
                        .f = ~bind_cols(.x,.y)))
##Creando tablas
   
   residence<-
     df3 %>% 
     select(year,residence) %>% 
     unnest(cols = c("residence")) %>% mutate(prop = round(prop*100, digits = 1),
                                        `2.5 %`=round(`2.5 %`*100, digits = 1),
                                        `97.5 %`=round(`97.5 %`*100, digits = 1),
                                        variable = "residence")

   natural_region<-
     df3 %>% 
     select(year,natural_region) %>% 
     unnest(cols = c("natural_region")) %>% mutate(prop = round(prop*100, digits = 1),
                                              `2.5 %`=round(`2.5 %`*100, digits = 1),
                                              `97.5 %`=round(`97.5 %`*100, digits = 1),
                                              variable = "natural_region")
   
   wealth_index<-
     df3 %>% 
     select(year,wealth_index) %>% 
     unnest(cols = c("wealth_index")) %>% mutate(prop = round(prop*100, digits = 1),
                                                   `2.5 %`=round(`2.5 %`*100, digits = 1),
                                                   `97.5 %`=round(`97.5 %`*100, digits = 1),
                                                   variable = "wealth_index")
   
   house_head<-
     df3 %>% 
     select(year,house_head) %>% 
     unnest(cols = c("house_head")) %>% mutate(prop = round(prop*100, digits = 1),
                                                  `2.5 %`=round(`2.5 %`*100, digits = 1),
                                                  `97.5 %`=round(`97.5 %`*100, digits = 1),
                                                  variable = "house_head")
   
   house_members<-
     df3 %>% 
     select(year,house_members) %>% 
     unnest(cols = c("house_members")) %>% mutate(prop = round(prop*100, digits = 1),
                                                 `2.5 %`=round(`2.5 %`*100, digits = 1),
                                                 `97.5 %`=round(`97.5 %`*100, digits = 1),
                                                 variable = "house_members")
   
   
   descriptivo2<-
     bind_rows(residence, natural_region, wealth_index, house_head, house_members) %>% 
     select(year,var,prop,`2.5 %`,`97.5 %`,variable)
   
   
   write.csv(descriptivo2,"./data/descriptivo_household.csv", row.names = F)
   
   
# VARIABLES AT PRENATAL CARE LEVEL
   
   df4 <- df %>% 
     mutate(
       
       insurance_ci = map(.x = datasvy,
                         .f = ~svymean(~as.factor(HEALTH_INSURANCE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                           confint() %>% 
                           as.data.frame() %>% 
                           rownames_to_column(var = "var")),
       
       insurance_m = map(.x = datasvy,
                         .f = ~svymean(~as.factor(HEALTH_INSURANCE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                           as.vector() %>% 
                           as.data.frame() %>% 
                           mutate("prop" = as.numeric(.))%>% 
                           select(-.)),

       insurance = map2(.x = insurance_m,
                        .y = insurance_ci,
                        .f = ~bind_cols(.x,.y)),
       
       complexity_ci = map(.x = datasvy,
                     .f = ~svymean(~as.factor(COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                       confint() %>% 
                       as.data.frame() %>% 
                       rownames_to_column(var = "var")),

       complexity_m = map(.x = datasvy,
                     .f = ~svymean(~as.factor(COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                       as.vector() %>% 
                       as.data.frame() %>% 
                       mutate("prop" = as.numeric(.))%>% 
                       select(-.)),
       
       complexity = map2(.x = complexity_m,
                        .y = complexity_ci,
                        .f = ~bind_cols(.x,.y)),
       
       
       institution_ci = map(.x = datasvy,
                      .f = ~svymean(~as.factor(PRENATAL_ATTENTION_PLACE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                        confint() %>% 
                        as.data.frame() %>% 
                        rownames_to_column(var = "var")),
       
       institution_m = map(.x = datasvy,
                          .f = ~svymean(~as.factor(PRENATAL_ATTENTION_PLACE), design = .x, na.rm = T, vartype = c("se","ci")) %>% 
                            as.vector() %>% 
                            as.data.frame() %>% 
                            mutate("prop" = as.numeric(.))%>% 
                            select(-.)),
       
       institution = map2(.x = institution_m,
                         .y = institution_ci,
                         .f = ~bind_cols(.x,.y)))
   
   
   insurance<-
     df4 %>% 
     select(year,insurance) %>% 
     unnest(cols = c("insurance")) %>% mutate(prop = round(prop*100, digits = 1),
                                                  `2.5 %`=round(`2.5 %`*100, digits = 1),
                                                  `97.5 %`=round(`97.5 %`*100, digits = 1),
                                                  variable = "insurance")
   complexity <-
     df4 %>% 
     select(year,complexity) %>% 
     unnest(cols = c("complexity")) %>% mutate(prop = round(prop*100, digits = 1),
                                                  `2.5 %`=round(`2.5 %`*100, digits = 1),
                                                  `97.5 %`=round(`97.5 %`*100, digits = 1),
                                                  variable = "complexity")
   institution<-
     df4 %>% 
     select(year,institution) %>% 
     unnest(cols = c("institution")) %>% mutate(prop = round(prop*100, digits = 1),
                                                  `2.5 %`=round(`2.5 %`*100, digits = 1),
                                                  `97.5 %`=round(`97.5 %`*100, digits = 1),
                                                  variable = "institution")
   
   
   descriptivo3<-
     bind_rows(insurance, complexity, institution) %>% 
     select(year,var,prop,`2.5 %`,`97.5 %`,variable)
   
   
   write.csv(descriptivo3,"./data/descriptivo_prenatal.csv", row.names = F)

# FIRTS PRENATAL VISIT  
       
   df22 <- df %>% mutate(first_visit_ci = map(.x = datasvy,
                       .f = ~svyquantile(~as.numeric(FIRST_PRENATAL_VISIT), design = .x, na.rm = T, 
                                         quantiles = c(0.25, 0.5, 0.75), ci=TRUE, interval.type="betaWald")))
   
   df22$first_visit_ci
      

# NUMBER OF PRENATAL CONTROL VISITS: NUMBER_PRENATAL_VISITS

df22 <- df %>% mutate(
  control_visit_ci = map(.x = datasvy,
                       .f = ~svyquantile(~as.numeric(NUMBER_PRENATAL_VISITS), design = .x, na.rm = T, 
                                         quantiles = c(0.25, 0.5,0.75), ci=TRUE, interval.type="betaWald")))

df22$control_visit_ci

##total
data_read<-read.csv("./data/data2021/data_2021final.csv")
attach(data_read)

install.packages("tmap")
library(tmap)

data_total <-data_read %>% mutate (EDU_LEVEL = recode(EDU_LEVEL, 'NONE/PRESCHOOL' = 'PRIMARY')) %>%
   mutate(ETHNICITY = recode(ETHNICITY, 'AIMARA' = 'QUECHUA')) %>% 
   mutate(ETHNICITY = recode(ETHNICITY, 'OTHER INDIGENOUS' = 'QUECHUA')) %>% 
   mutate(across(where(is.character), ~na_if(., "FOREIGNER"))) %>% 
   mutate(PRENATAL_ATTENTION_PLACE = recode(PRENATAL_ATTENTION_PLACE, 'FF.AA.' = 'OTHERS')) %>% 
   mutate(PRENATAL_ATTENTION_PLACE = recode(PRENATAL_ATTENTION_PLACE, 'PRIVATE' = 'OTHERS')) %>% 
   filter(LAST_BIRTH== "LESS THAN 12 MONTHS") %>% 
   filter(IDX94=="1")

#----------------------------------------
# Transformando la data en objeto survey
#----------------------------------------
data(data_total)
df_total<- svydesign(id =~ V001, strata =~ V022, weights=~V005, data=data_total, nest = TRUE)

prop.table(svytable(~AGE_MOTHER, df_total))

prop.table(svytable(~ETHNICITY, df_total))

prop.table(svytable(~KNOW_ETS, df_total))


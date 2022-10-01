library(ggradar)
library(tidyverse)
library(scales)


df_gestantes<- read.csv("./data/datatest.csv")

###################
df_gestantes2<-
  df_gestantes %>% 
  
  mutate(
    CHECKUP_RULE_OUT_HIV = ifelse(is.na(CHECKUP_RULE_OUT_HIV),"NO",CHECKUP_RULE_OUT_HIV)
  )

##################
df<-
  df_gestantes2 %>% 
  mutate(
    CHECKUP_RULE_OUT_HIV2 = ifelse(is.na(CHECKUP_RULE_OUT_HIV),"Missing",
                                   ifelse(CHECKUP_RULE_OUT_HIV == 1,"Yes",
                                          ifelse(CHECKUP_RULE_OUT_HIV == 0 | CHECKUP_RULE_OUT_HIV == 8 , "No",CHECKUP_RULE_OUT_HIV))),
    
    CHECKUP_RULE_OUT_HIV2 = as.factor(CHECKUP_RULE_OUT_HIV2)
  ) %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    datasvy = map(.x = data,
                  .f = ~svydesign(id =~ V001, strata =~ V022, weights=~V005, data=.x))
  )
options(survey.lonely.psu="remove")

#################

df2<-
  df %>% 
  mutate(
    vih = map(.x = datasvy,
              .f = ~svyby(~as.factor(CHECKUP_RULE_OUT_HIV), by = ~as.factor(DEPARTAMEN), design = .x, FUN =svyciprop, na.rm.all = T))
  ) %>% 
  unnest(vih) %>% 
  
  mutate(
    across(`as.factor(CHECKUP_RULE_OUT_HIV)`:`se.as.numeric(as.factor(CHECKUP_RULE_OUT_HIV))`,.fns = ~round(.,2)*100)
  ) %>%
  
  rename(
    DEPARTAMEN = `as.factor(DEPARTAMEN)`,
    `HIV Screening` = `as.factor(CHECKUP_RULE_OUT_HIV)`
  )
                        

##### Grafico ###########
df2 %>% 
  select(year,`HIV Screening`,DEPARTAMEN) %>% 
  
  
  
  group_by(DEPARTAMEN,year) %>% 
  
  arrange(.by_group = T) %>% 
  
  
  pivot_wider(names_from = "year", values_from = `HIV Screening`) %>% 
  
  ggradar(grid.max = 100)


##########################
df3<-
  df2 %>% 
  select(year,`HIV Screening`,DEPARTAMEN) %>% 
  
  
  
  group_by(DEPARTAMEN,year) %>% 
  
  arrange(.by_group = T) %>% 
  
  
  pivot_wider(names_from = "year", values_from = `HIV Screening`) %>% 

  
  mutate(
    
    macroreg = ifelse(DEPARTAMEN == "TUMBES"|DEPARTAMEN == "PIURA"|
                        DEPARTAMEN == "LAMBAYEQUE"|DEPARTAMEN == "LA LIBERTAD"|
                        DEPARTAMEN == "CAJAMARCA"|DEPARTAMEN == "AMAZONAS"|
                        DEPARTAMEN == "SAN MARTIN","Macro NO",
                      
                      ifelse(DEPARTAMEN == "ANCASH"|DEPARTAMEN == "UCAYALI"|
                               DEPARTAMEN == "JUNIN"|DEPARTAMEN == "HUANUCO"|
                               DEPARTAMEN == "PASCO"|DEPARTAMEN == "HUANCAVELICA","MACRO C",
                             
                             ifelse(DEPARTAMEN == "ICA"|DEPARTAMEN == "AREQUIPA"|
                                       DEPARTAMEN == "MOQUEGUA"|DEPARTAMEN == "TACNA","Macro SO",
                                     
                                     ifelse(DEPARTAMEN == "CUSCO"|DEPARTAMEN == "PUNO"|
                                              DEPARTAMEN == "AYACUCHO"|DEPARTAMEN == "MADRE DE DIOS"|
                                              DEPARTAMEN == "APURIMAC","Macro SE",
                                            
                                            ifelse(DEPARTAMEN == "LORETO", "macro NE",
                                                   
                                                   ifelse(DEPARTAMEN == "LIMA"|
                                                            DEPARTAMEN == "CALLAO","Macro Lima",DEPARTAMEN))))))
  ) 
  
  
df3 %>% 
  group_by(macroreg) %>% 
  
  nest() %>% 
  
  mutate(
    grafico = map(.x = data,
                  .f = ~ggradar(plot.data = .x,
                                grid.max = 100,
                                grid.min = 0,
                                grid.mid = 50,
                                values.radar = c("10","30","50","70","100")) 
    )
  )


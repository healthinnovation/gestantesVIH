library(ggradar)
library(tidyverse)
library(scales)
library(survey)
library(fmsb)

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
  
  ungroup() %>% 

  
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
  
  
color <- c("#5F4690","#1D6996","#38A6A5","#0F8554",
           "#73AF48","#EDAD08","#E17C05","#CC503E",
           "#94346E","#6F4070","#994E95","#666666")

blue_fall <- c("#eaac8b","#e56b6f","#b56576","#6d597a", "#355070","red"
)
a<-
  df3 %>% 
  
  #select(-DEPARTAMEN) %>% 
  
  mutate(
    macroreg = as.character(macroreg),
    DEPARTAMEN = as.character(DEPARTAMEN)
  ) %>% 
  
  group_by(macroreg) %>% 
  
  nest() %>% 
  
  mutate(
    
    grafico = map(.x = data,
                  .f = ~ggradar(.x, grid.max = 100, 
                                legend.text.size = 8, 
                                legend.position = "bottom", group.colours = color))
                    
                    )


cowplot::plot_grid(plotlist =  a$grafico, ncol = 3, labels = "auto")

ggsave("test.jpg",width = 22, height = 11, dpi = 300)
  
a$grafico[[1]]


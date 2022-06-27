library(tidyverse)
library(survey)
library(gtsummary)
library(ggsci)

df_gestantes<- read.csv("./data/data_analysis_2021.csv")

df<-
  df_gestantes %>% 
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


df2<-
  df %>% 
  mutate(
    
    vih = map(.x = datasvy,
              .f = ~svymean(~as.factor(CHECKUP_RULE_OUT_HIV2), design = .x, na.rm = T)),
    
    vihprop = map(.x = vih,
                  .f = ~as.numeric(.x)),
    
    vihci = map(.x= vih,
                .f = ~confint(.x) %>% 
                  as.data.frame() %>%
                  rownames_to_column("var"))
    
  ) %>% 
  
  unnest(c(vihci,vihprop)) %>% 
  
  mutate(
    var = case_when(var == "as.factor(CHECKUP_RULE_OUT_HIV2)Missing" ~ "Missing",
                    var == "as.factor(CHECKUP_RULE_OUT_HIV2)YES" ~ "Yes",
                    var == "as.factor(CHECKUP_RULE_OUT_HIV2)NO" ~ "No")
  )


##### Lines

ggplot(df2, aes(x = year, y = vihprop*100, ymin = `2.5 %`*100, ymax = `97.5 %`*100, group = var)) + 
  geom_line(size = 1.2, aes(col = var))+
  geom_ribbon(aes(fill = var), alpha = 0.1)+
  
  xlab("Years") +
  scale_x_continuous(expand = c(0, 0), breaks = c(2010:2021), limits = c(2010,2021.1))+
  ylab("Prop. %")+
  
  theme_bw()+
  
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(size = 8, face = "bold"),
    axis.text = element_text(face = "bold", family = "Arial"),
    axis.text.x = element_text( size = 9),
    legend.position = "top",
    legend.title = element_blank(),
    axis.line = element_line(colour = "black", size = 1),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )

#ggsave("figura1_2.png",width = 8, height = 6, dpi= 300)
##### Bar

ggplot(df2, aes(x = year, y = vihprop*100, group = var)) + 
  geom_col(size = 1.2, aes(fill = var))+
  ggsci::scale_fill_futurama(alpha = 0.8)+
  
  xlab("Years") +
  scale_x_continuous(expand = c(0, 0), breaks = c(2010:2021), limits = c(2009.5,2021.6))+
  ylab("Prop. %")+
  
  theme_bw()+
  
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    axis.text = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )
  

ggsave("figura1.png",width = 8, height = 6, dpi= 300)


################
# df2 %>%
#   filter(year%in%c(2010,2020)) %>%
#   ggplot(aes(x = year, y = vihprop*100, group = var))+
#   geom_point(color = "#0C6291", size = 6)+
#   geom_line(color = "#0C6291", size = 1.5)+
# 
#   xlab("Years") +
#   scale_x_continuous(expand = c(0, 0), breaks = c(2010:2020), limits = c(2009,2022))+
#   ylab("Prop. %")+
# 
#   theme_bw()+
# 
#   theme(
#     axis.title = element_text(face ="bold", size = 11),
#     legend.text = element_text(face = "bold", size = 12),
#     legend.position = "top",
#     strip.text = element_text(face = "bold")
#   )
# 
# ggsave("figura2.png",width = 8, height = 6)


df2<-
  df %>% 
  mutate(
    vih = map(.x = datasvy,
              .f = ~svyby(~as.factor(CHECKUP_RULE_OUT_HIV), by = ~WEALTH_INDEX, design = .x, FUN =svyciprop, vartype=c('se','ci')) %>% 
                as.data.frame())
  ) %>% 
  unnest(vih)

df2 %>% 
  ggplot(aes(x = year, y = `as.factor(CHECKUP_RULE_OUT_HIV)`*100, ymin = ci_l*100, ymax = ci_u*100))+
  geom_line(size = 1.2, aes(col = WEALTH_INDEX))+
  geom_ribbon(aes(fill = WEALTH_INDEX), alpha = 0.1)+
  scale_color_nejm()+
  scale_fill_nejm()+
  xlab("Years") +
  scale_x_continuous(expand = c(0, 0), breaks = c(2010:2021), limits = c(2010,2021.2))+
  ylab("Prop. %")+
  theme_bw()+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(size = 8, face = "bold"),
    axis.text = element_text(face = "bold", family = "Arial"),
    axis.text.x = element_text( size = 9),
    legend.position = "top",
    legend.title = element_blank(),
    axis.line = element_line(colour = "black", size = 1)
  )

ggsave("figura2.png",dpi= 300, width = 8, height = 6)
  

# df2<-
#   df %>% 
#   mutate(
#     vih = map(.x = datasvy,
#               .f = ~svyby(~as.factor(CHECKUP_RULE_OUT_HIV), by = ~INDICERIQUEZA+TIPORESIDENCIA, design = .x, FUN =svyciprop, vartype=c('se','ci')) %>% 
#                 as.data.frame())
#   ) %>% 
#   unnest(vih)
# 
# dat<-
#   df2 %>% 
#   filter((year%in%c(2010,2020))&INDICERIQUEZA%in%c("1ro","5to")) %>% 
#   mutate(
#     WI = paste0(INDICERIQUEZA,TIPORESIDENCIA),
#     line = ifelse((INDICERIQUEZA == "1ro"| INDICERIQUEZA == "5to") & TIPORESIDENCIA == "RURAL", 1, 0)
#   )
# 
# 
#   ggplot()+
#   geom_line(data = dat %>% filter(TIPORESIDENCIA == "RURAL"), 
#             aes(x = year, y = `as.factor(S411H)`*100, group = WI,col = WI, linetype = TIPORESIDENCIA))+
#     
#   geom_point(data = dat %>% filter(TIPORESIDENCIA == "RURAL"), 
#              aes(x = year, y = `as.factor(S411H)`*100, group = WI, col = WI, fill = TIPORESIDENCIA),size = 2)+
#     
#     
#   geom_line(data = dat %>% filter(TIPORESIDENCIA == "URBANO"), 
#               aes(x = year, y = `as.factor(S411H)`*100, group = WI, col = WI, linetype = TIPORESIDENCIA))+
# 
#   geom_point(data = dat %>% filter(TIPORESIDENCIA == "URBANO"), 
#                aes(x = year, y = `as.factor(S411H)`*100, group = WI, col = WI, fill = TIPORESIDENCIA),size = 2)+
#     
#   scale_color_npg()+
#   labs(x="Years", col = "Wealth Index") +
#   #guides(linetype = "none")+
#   scale_x_continuous(expand = c(0, 0), breaks = c(2010:2020), limits = c(2009.8,2020.5))+
#   ylab("Prop. %")+
#     theme_bw()
# 
# 
# ggsave("figura5.png",width = 8, height = 6)




df2<-
  df %>% 
  mutate(
    vih = map(.x = datasvy,
              .f = ~svyby(~as.factor(CHECKUP_RULE_OUT_HIV), by = ~as.factor(DEPARTAMEN), design = .x, FUN =svyciprop, na.rm.all = T))
  ) %>% 
  unnest(vih)

df2

df2 %>% 
  ggplot(aes(x = year, y = `as.factor(CHECKUP_RULE_OUT_HIV)`*100))+
  geom_line()+
  geom_point()+
  scale_color_npg()+
  xlab("Years") +
  scale_x_continuous(expand = c(0, 0), breaks = c(2010:2021), limits = c(2009.8,2021.5))+
  ylab("Prop. %") +
  facet_wrap(~DEPARTAMEN)

ggsave("figura3.png",width = 13, height = 7)


df2<-
  df %>% 
  mutate(
    vih = map(.x = datasvy,
              .f = ~svyby(~as.factor(CHECKUP_RULE_OUT_HIV), by = ~EDU_LEVEL, design = .x, FUN =svyciprop, vartype=c('se','ci')) %>% 
                as.data.frame())
  ) %>% 
  unnest(vih)

df2 %>% 
 
  ggplot(aes(x = year, y = `as.factor(S411H)`*100, col = EDU_MADRE))+
  geom_line(size = 1.2)+
  scale_color_npg()+
  xlab("Years") +
  scale_x_continuous(expand = c(0, 0), breaks = c(2010:2020), limits = c(2010,2020.2))+
  ylab("Prop. %")

ggsave("figura4.png",width = 8, height = 6)






library(tidyverse)
library(survey)
library(gtsummary)
library(ggsci)

df_gestantes<- read.csv("./data/data_analysis.csv")

df<-
  df_gestantes %>% 
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
              .f = ~svyciprop(~as.factor(S411H), design = .x, na.rm = T)),
    
    vihprop = map_dbl(.x = vih,
                  .f = ~as.numeric(.x)),
    
    vihci = map(.x= vih,
                .f = ~confint(.x) %>% 
                  as.data.frame() %>%
                  rownames_to_column("var"))

  ) %>% 
  
  unnest(vihci)


df2 %>% 
  ggplot(aes(x = year, y = vihprop*100))+
  geom_line(size = 1.2, color = "#0C6291")+
  
  xlab("Years") +
  scale_x_continuous(expand = c(0, 0), breaks = c(2010:2020), limits = c(2010,2020.1))+
  ylab("Prop. %")+
  
  theme_bw()+
  
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(size = 8, face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )

ggsave("figura1.png",width = 8, height = 6)



df2 %>% 
  filter(year%in%c(2010,2020)) %>% 
  ggplot(aes(x = year, y = vihprop*100))+
  geom_point(color = "#0C6291", size = 6)+
  geom_line(color = "#0C6291", size = 1.5)+
  
  xlab("Years") +
  scale_x_continuous(expand = c(0, 0), breaks = c(2010:2020), limits = c(2009,2022))+
  ylab("Prop. %")+
  
  theme_bw()+
  
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )

ggsave("figura2.png",width = 8, height = 6)


df2<-
  df %>% 
  mutate(
    vih = map(.x = datasvy,
              .f = ~svyby(~as.factor(S411H), by = ~INDICERIQUEZA, design = .x, FUN =svyciprop, vartype=c('se','ci')) %>% 
                as.data.frame())
  ) %>% 
  unnest(vih)

df2 %>% 
  ggplot(aes(x = year, y = `as.factor(S411H)`*100, col = INDICERIQUEZA))+
  geom_line(size = 1.2)+
  scale_color_npg()+
  xlab("Years") +
  scale_x_continuous(expand = c(0, 0), breaks = c(2010:2020), limits = c(2010,2020.2))+
  ylab("Prop. %")

ggsave("figura3.png",width = 8, height = 6)
  
df2 %>% 
  filter((year%in%c(2010,2020))&INDICERIQUEZA%in%c("1ro","5to")) %>% 
  ggplot(aes(x = year, y = `as.factor(S411H)`*100, col = INDICERIQUEZA))+
  geom_line(size = 1.6)+
  geom_point(size = 6)+
  scale_color_npg()+
  xlab("Years") +
  scale_x_continuous(expand = c(0, 0), breaks = c(2010:2020), limits = c(2009.8,2020.5))+
  ylab("Prop. %")


ggsave("figura4.png",width = 8, height = 6)




df2<-
  df %>% 
  mutate(
    vih = map(.x = datasvy,
              .f = ~svyby(~as.factor(S411H), by = ~INDICERIQUEZA+TIPORESIDENCIA, design = .x, FUN =svyciprop, vartype=c('se','ci')) %>% 
                as.data.frame())
  ) %>% 
  unnest(vih)

dat<-
  df2 %>% 
  filter((year%in%c(2010,2020))&INDICERIQUEZA%in%c("1ro","5to")) %>% 
  mutate(
    WI = paste0(INDICERIQUEZA,TIPORESIDENCIA),
    line = ifelse((INDICERIQUEZA == "1ro"| INDICERIQUEZA == "5to") & TIPORESIDENCIA == "RURAL", 1, 0)
  )


  ggplot()+
  geom_line(data = dat %>% filter(TIPORESIDENCIA == "RURAL"), 
            aes(x = year, y = `as.factor(S411H)`*100, group = WI,col = WI, linetype = TIPORESIDENCIA))+
    
  geom_point(data = dat %>% filter(TIPORESIDENCIA == "RURAL"), 
             aes(x = year, y = `as.factor(S411H)`*100, group = WI, col = WI, fill = TIPORESIDENCIA),size = 2)+
    
    
  geom_line(data = dat %>% filter(TIPORESIDENCIA == "URBANO"), 
              aes(x = year, y = `as.factor(S411H)`*100, group = WI, col = WI, linetype = TIPORESIDENCIA))+

  geom_point(data = dat %>% filter(TIPORESIDENCIA == "URBANO"), 
               aes(x = year, y = `as.factor(S411H)`*100, group = WI, col = WI, fill = TIPORESIDENCIA),size = 2)+
    
  scale_color_npg()+
  labs(x="Years", col = "Wealth Index") +
  #guides(linetype = "none")+
  scale_x_continuous(expand = c(0, 0), breaks = c(2010:2020), limits = c(2009.8,2020.5))+
  ylab("Prop. %")+
    theme_bw()


ggsave("figura5.png",width = 8, height = 6)




df2<-
  df %>% 
  mutate(
    vih = map(.x = datasvy,
              .f = ~svyby(~as.factor(S411H), by = ~INDICERIQUEZA+DEPARTAMEN, design = .x, FUN =svyciprop, vartype=c('se','ci'), na.rm.all = T) %>% 
                as.data.frame())
  ) %>% 
  unnest(vih)

dat<-
  df2 %>% 
  filter((year%in%c(2010,2020))&INDICERIQUEZA%in%c("1ro","5to"))

dat %>% 
  ggplot(aes(x = year, y = `as.factor(S411H)`*100, col = INDICERIQUEZA))+
  geom_line()+
  geom_point()+
  scale_color_npg()+
  xlab("Years") +
  scale_x_continuous(expand = c(0, 0), breaks = c(2010:2020), limits = c(2009.8,2020.5))+
  ylab("Prop. %") +
  facet_wrap(~DEPARTAMEN)

ggsave("figura6.png",width = 13, height = 7)




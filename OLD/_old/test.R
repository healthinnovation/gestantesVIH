df2<-
  df %>% 
  mutate(
    vihcount = map(.x = datasvy,
                   .f = ~svyciprop(~as.factor(S411H), design = .x, na.rm = T)),
    
    vih = map_dbl(.x = vihcount,
              .f = ~as.vector(.x)),
    
    
    
    hivci = map(.x = vihcount,
                 .f = ~confint(.x) %>% 
                   as.data.frame() %>%
                   rownames_to_column("test"))) %>% 
  
  unnest(cols = c(hivci)) %>% 
  
  select(year,vih,`2.5%`,`97.5%`)


df2 %>% 
  ggplot(aes(x = year, y = vih*100,
             ymin = `2.5%`*100, ymax = `97.5%`*100))+
  
  geom_line(size = 1.5, color = "#6574A3") +
  geom_ribbon(alpha = 0.1, fill = "#6574A3")+
  
  xlab("Years") +
  scale_x_continuous(expand = c(0, 0), breaks = c(2010:2020), limits = c(2010,2020.1))+
  ylab("Prop. %")+
  theme_bw()

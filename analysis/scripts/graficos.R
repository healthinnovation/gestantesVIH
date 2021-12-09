library(tidyverse)
library(survey)
library(gtsummary)

df<-
  dataset %>% 
  filter(!is.na(checkup_rule_out_hiv_aids)) %>% 
  group_by(year) %>%
  nest() %>% 
  mutate(
    df2= map(.x = data, 
            .f = ~svydesign(id =~ conglomerate, strata =~ stratum, weights=~weighting_factor, data=.x))) 


options(survey.lonely.psu="remove")


tamizaje<-df %>% 
  mutate(
    
    tamizaje = map(.x = df2,
                   .f = ~svytotal(~checkup_rule_out_hiv_aids, design = .x, na.rm = T ))
  )


################

dataset %>% 
  filter(!is.na(checkup_rule_out_hiv_aids)) %>% 
  select(year, checkup_rule_out_hiv_aids) %>% 
  group_by(year,checkup_rule_out_hiv_aids) %>% 
  summarise(
    n = n()
  ) %>% 
  ggplot(aes(x = year, y = n, col = checkup_rule_out_hiv_aids))+
  geom_line()+
  theme_bw()



#######

dataset %>% 
  filter(!is.na(checkup_rule_out_syphilis)) %>% 
  select(year, checkup_rule_out_syphilis) %>% 
  group_by(year,checkup_rule_out_syphilis) %>% 
  summarise(
    n = n()
  ) %>% 
  
  ggplot(aes(x = year, y = n, col = checkup_rule_out_syphilis))+
  geom_line()+
  theme_bw()


########

# tamizaje por WI

dataset %>% 
  filter(!is.na(checkup_rule_out_hiv_aids)) %>% 
  select(year, checkup_rule_out_hiv_aids,wealth_index) %>% 
  group_by(year,checkup_rule_out_hiv_aids,wealth_index) %>% 
  summarise(
    n = n()
  ) %>% 
  
  mutate(
    wealth_index = factor(wealth_index, labels = c("Poorest","Poor","Middle",
                                                   "Rich","Richest"))
  ) %>% 
  ggplot(aes(x = year, y = n, col = checkup_rule_out_hiv_aids))+
  geom_line()+
  theme_bw()+
  facet_wrap(~wealth_index)


# tamizaje por region

dataset %>% 
  filter(!is.na(checkup_rule_out_hiv_aids)) %>% 
  group_by(region, checkup_rule_out_hiv_aids) %>% 
  summarize(cases = n()) %>% 
  mutate(prop = cases / sum(cases)) %>% 
  ungroup() %>% 
  ggplot(aes(region, prop, fill = checkup_rule_out_hiv_aids)) +
  geom_bar(stat = "identity", position = "stack") + 
  #scale_y_continuous(labels = percent) +
  coord_flip()

#######
# tamizaje por educacion

dataset %>% 
  filter(!is.na(checkup_rule_out_hiv_aids)) %>% 
  group_by(highest_edu_level_woman, checkup_rule_out_hiv_aids) %>% 
  summarize(cases = n()) %>% 
  mutate(prop = cases / sum(cases)) %>% 
  ungroup() %>% 
  mutate(
    edu = factor(highest_edu_level_woman, labels = c("No education","Primary",
                                                     "Secondary","Higher")
  )) %>% 
  ggplot(aes(edu, prop, fill = checkup_rule_out_hiv_aids)) +
  geom_bar(stat = "identity", position = "stack") + 
  #scale_y_continuous(labels = percent) +
  coord_flip()

# tamizaje por seguro de salud

dataset %>% 
  filter(!is.na(checkup_rule_out_hiv_aids)) %>% 
  group_by(pregnancy_sis_affiliation, checkup_rule_out_hiv_aids,year) %>% 
  summarize(cases = n()) %>% 
  mutate(prop = cases / sum(cases)) %>% 
  ungroup() %>% 
  ggplot(aes(pregnancy_sis_affiliation, prop, fill = checkup_rule_out_hiv_aids)) +
  geom_col(stat = "identity", position = "stack") + 
  #scale_y_continuous(labels = percent) +
  coord_flip()+
  facet_wrap(~year)

# tamizaje por educacion/region

# tamizaje por educacion

dataset %>% 
  filter(!is.na(checkup_rule_out_hiv_aids)) %>% 
  group_by(highest_edu_level_woman, checkup_rule_out_hiv_aids,region) %>% 
  summarize(cases = n()) %>% 
  mutate(prop = cases / sum(cases)) %>% 
  ungroup() %>% 
  mutate(
    edu = factor(highest_edu_level_woman, labels = c("No education","Primary",
                                                     "Secondary","Higher")
    )) %>% 
  ggplot(aes(edu, prop, fill = checkup_rule_out_hiv_aids)) +
  geom_bar(stat = "identity", position = "stack") + 
  #scale_y_continuous(labels = percent) +
  coord_flip()+
  facet_wrap(~region)+
  theme(
    panel.spacing.x = unit(2, "line")
  )

ggsave("tamizaje.png", width = 16, height = 6)


# tamizaje por violencia fisica durante embarazo

dataset %>% 
  filter(!is.na(checkup_rule_out_hiv_aids)& !is.na(physical_violence_pregnancy_nobody)) %>% 
  group_by(physical_violence_pregnancy_nobody, checkup_rule_out_hiv_aids,region) %>% 
  summarize(cases = n()) %>% 
  mutate(prop = cases / sum(cases)) %>% 
  ungroup() %>% 
  ggplot(aes(physical_violence_pregnancy_nobody, prop, fill = checkup_rule_out_hiv_aids)) +
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip()+
  facet_wrap(~region)

# tamizaje por violencia emocional

dataset %>% 
  filter(!is.na(checkup_rule_out_hiv_aids)) %>% 
  group_by(emotional_violence, checkup_rule_out_hiv_aids) %>% 
  summarize(cases = n()) %>% 
  mutate(prop = cases / sum(cases)) %>% 
  ungroup() %>% 
  ggplot(aes(emotional_violence, prop, fill = checkup_rule_out_hiv_aids)) +
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip()

# tamizaje por violencia sexual


dataset %>% 
  filter(!is.na(checkup_rule_out_hiv_aids)) %>% 
  group_by(sexual_violence, checkup_rule_out_hiv_aids) %>% 
  summarize(cases = n()) %>% 
  mutate(prop = cases / sum(cases)) %>% 
  ungroup() %>% 
  ggplot(aes(sexual_violence, prop, fill = checkup_rule_out_hiv_aids)) +
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip()


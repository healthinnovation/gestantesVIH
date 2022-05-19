library(tidyverse)
library(scales)
library(ggplot2)
library(caret)
data_read<-read.csv("./data/datafinal.csv")
head(data_read)
names(data_read)

data <- data_read %>% select( "year", "AGE_MOTHER", "WEALTH_INDEX", "RELATIONSHIP_HOUSEHOLD_HEAD", "TYPE_PLACE_RESIDENCE",                       
                      "ETHNICITY", "DEPARTAMEN", "LITERACY", "CURRENT_MARITAL_STATUS",  "EDU_LEVEL", "NATURAL_REGION", "PARTNER_APPROVE_DISAPPROVE_FAMILY_PLANNING", 
                      "PARTNER_EDU_LEVEL", "CAN_SOMETHING_BE_DONE_PREVENT_AIDS", "KNOW_ETS", "KNOW_SYMPTON_ETS", "CHECKUP_RULE_OUT_SYPHILIS",
                      "CHECKUP_RULE_OUT_HIV", "PRENATAL_CARE_ATTENTION",  "INTENDED_PREGNANCY", "PHYSICAL_VIOLENCE",
                      "DIAGNOSTED_STD_LAST_12_MONTHS", "KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD", "HOUSEHOLD_MEMBERS",                         
                      "FIRST_PRENATAL_VISIT", "NUMBER_PRENATAL_VISITS", "PRENATAL_ATTENTION_PLACE", 
                      "COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE", "HAVE_ITS_SYMPTOMS",  "TOTAL_CHILDREN", 
                      "UNDER_SIXYEARS_CHILDREN", "HEALTH_INSURANCE", "LAST_BIRTH")

#variable respuesta : CHECKUP_RULE_OUT_HIV  

str(data)
summary(data)
glimpse(data)

##################################
# ANALISIS UNIVARIADO: NUM?RICAS
##################################

#----------------------
# FIRST PRENATAL VISIT
#----------------------
ggplot(data, aes(x=FIRST_PRENATAL_VISIT)) + geom_histogram(binwidth=.5) + facet_wrap(~year)

#------------------------
# NUMBER PRENATAL VISITS
#------------------------
ggplot(data, aes(x=NUMBER_PRENATAL_VISITS)) + geom_histogram(binwidth=.5) + facet_wrap(~year)

#-----------------
# TOTAL CHILDREN
#-----------------
ggplot(data, aes(x=TOTAL_CHILDREN)) + geom_histogram(binwidth=.5) + facet_wrap(~year)

#-------------------------
# UNDER SIXYEARS CHILDREN
#-------------------------
ggplot(data, aes(x=UNDER_SIXYEARS_CHILDREN)) + geom_histogram(binwidth=.5) + facet_wrap(~year)

####################################
# ANALISIS UNIVARIADO: CATEG?RICAS
####################################

#-------------
# AGE MOTHER
#-------------
plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, AGE_MOTHER) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)


ggplot(plot_data, aes(x = factor(AGE_MOTHER), fill = factor(AGE_MOTHER), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent)+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de Edad de la madre por periodo")

table(data$year, data$AGE_MOTHER)[11,]*100/sum(table(data$year, data$AGE_MOTHER)[11,])

#--------------
# WEALTH_INDEX
#--------------
plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, WEALTH_INDEX) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)


ggplot(plot_data, aes(x = factor(WEALTH_INDEX), fill = factor(WEALTH_INDEX), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent)+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporci?n de Indice de riqueza por periodo")

#--------------
# RELATIONSHIP_HOUSEHOLD_HEAD
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year,RELATIONSHIP_HOUSEHOLD_HEAD) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)


ggplot(plot_data, aes(x = factor(RELATIONSHIP_HOUSEHOLD_HEAD), fill = factor(RELATIONSHIP_HOUSEHOLD_HEAD), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent)+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de relación con la cabeza del hogar por periodo")

#--------------
# TYPE_PLACE_RESIDENCE  
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year,TYPE_PLACE_RESIDENCE) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)


ggplot(plot_data, aes(x = factor(TYPE_PLACE_RESIDENCE), fill = factor(TYPE_PLACE_RESIDENCE), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent)+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de tipo de lugar de residencia por periodo")


#--------------
# ETHNICITY  
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year,ETHNICITY) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)


ggplot(plot_data, aes(x = factor(ETHNICITY), fill = factor(ETHNICITY), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent)+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de etnicidad por periodo")

#--------------
# DEPARTAMEN  
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, DEPARTAMEN) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)


ggplot(plot_data, aes(x = factor(DEPARTAMEN), fill = factor(DEPARTAMEN), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent)+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de departamento por periodo")

#--------------
# LITERACY  
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, LITERACY) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(LITERACY), fill = factor(LITERACY), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent)+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de alfabetización por periodo")

#--------------
# CURRENT_MARITAL_STATUS
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year,CURRENT_MARITAL_STATUS) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(CURRENT_MARITAL_STATUS), fill = factor(CURRENT_MARITAL_STATUS), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent)+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de estado civil por periodo")

#--------------
# EDU_LEVEL
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, EDU_LEVEL) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(EDU_LEVEL), fill = factor(EDU_LEVEL), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent)+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de nivel educativo por periodo")

#--------------
# NATURAL_REGION
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, NATURAL_REGION) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(NATURAL_REGION), fill = factor(NATURAL_REGION), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent)+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de regiones naturales por periodo")

#--------------
# PARTNER_APPROVE_DISAPPROVE_FAMILY_PLANNING
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, PARTNER_APPROVE_DISAPPROVE_FAMILY_PLANNING) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(PARTNER_APPROVE_DISAPPROVE_FAMILY_PLANNING), fill = factor(PARTNER_APPROVE_DISAPPROVE_FAMILY_PLANNING), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de opinión de planificación familiar de la pareja por periodo")

#--------------
# PARTNER_EDU_LEVEL
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, PARTNER_EDU_LEVEL) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(PARTNER_EDU_LEVEL), fill = factor(PARTNER_EDU_LEVEL), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de nivel educativo de la pareja por periodo")

#--------------
# CAN_SOMETHING_BE_DONE_PREVENT_AIDS
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, CAN_SOMETHING_BE_DONE_PREVENT_AIDS) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(CAN_SOMETHING_BE_DONE_PREVENT_AIDS), fill = factor(CAN_SOMETHING_BE_DONE_PREVENT_AIDS), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de prevención de SIDA por periodo")

#--------------
# KNOW_ETS
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, KNOW_ETS) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(KNOW_ETS), fill = factor(KNOW_ETS), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de conocimiento de ETS por periodo")

#--------------
# KNOW_SYMPTON_ETS
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, KNOW_SYMPTON_ETS) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(KNOW_SYMPTON_ETS), fill = factor(KNOW_SYMPTON_ETS), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de conocimiento de síntomas de ETS por periodo")

#--------------
# CHECKUP_RULE_OUT_SYPHILIS
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, CHECKUP_RULE_OUT_SYPHILIS) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(CHECKUP_RULE_OUT_SYPHILIS), fill = factor(CHECKUP_RULE_OUT_SYPHILIS), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de tamizaje de sífilis por periodo")

#--------------
# CHECKUP_RULE_OUT_HIV
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, CHECKUP_RULE_OUT_HIV) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(CHECKUP_RULE_OUT_HIV), fill = factor(CHECKUP_RULE_OUT_HIV), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de tamizaje de VIH por periodo")

#--------------
# PRENATAL_CARE_ATTENTION
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, PRENATAL_CARE_ATTENTION) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(PRENATAL_CARE_ATTENTION), fill = factor(PRENATAL_CARE_ATTENTION), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de atención de cuidado prenatal por periodo")

#--------------
# INTENDED_PREGNANCY
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, INTENDED_PREGNANCY) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(INTENDED_PREGNANCY), fill = factor(INTENDED_PREGNANCY), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de embarazo intencionado por periodo")

#--------------
# PHYSICAL_VIOLENCE
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, PHYSICAL_VIOLENCE) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(PHYSICAL_VIOLENCE), fill = factor(PHYSICAL_VIOLENCE), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de violencia física por periodo")

#--------------
# DIAGNOSTED_STD_LAST_12_MONTHS
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, DIAGNOSTED_STD_LAST_12_MONTHS) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(DIAGNOSTED_STD_LAST_12_MONTHS), fill = factor(DIAGNOSTED_STD_LAST_12_MONTHS), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de diagnóstico de ETS durante los últimos 12 meses por periodo")

#--------------
# KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD), fill = factor(KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de conocimiento de tramisión de VIH de madre a hijo por periodo")

#--------------
# HOUSEHOLD_MEMBERS
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, HOUSEHOLD_MEMBERS) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(HOUSEHOLD_MEMBERS), fill = factor(HOUSEHOLD_MEMBERS), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de número de miembros del hogar por periodo")

#--------------
# PRENATAL_ATTENTION_PLACE
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, PRENATAL_ATTENTION_PLACE) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(PRENATAL_ATTENTION_PLACE), fill = factor(PRENATAL_ATTENTION_PLACE), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de lugares de atención prenatal por periodo")

#--------------
# COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE), fill = factor(COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de lugares de atención prenatal por periodo")

#--------------
# HAVE_ITS_SYMPTOMS
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, HAVE_ITS_SYMPTOMS) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(HAVE_ITS_SYMPTOMS), fill = factor(HAVE_ITS_SYMPTOMS), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de mujeres con síntomas de ETS por periodo")

#--------------
# HEALTH_INSURANCE
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, HEALTH_INSURANCE) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(HEALTH_INSURANCE), fill = factor(HEALTH_INSURANCE), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de afiliación a seguros de salud por periodo")

#--------------
# LAST_BIRTH
#--------------

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, LAST_BIRTH) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(LAST_BIRTH), fill = factor(LAST_BIRTH), y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de hijos de menos de 12 meses")

############################
# PRE-PROCESAMIENTO
############################


data1 <- data %>% select( "year", "AGE_MOTHER", "WEALTH_INDEX", "RELATIONSHIP_HOUSEHOLD_HEAD", "TYPE_PLACE_RESIDENCE",                       
                              "ETHNICITY", "DEPARTAMEN", "EDU_LEVEL", "NATURAL_REGION", "PARTNER_EDU_LEVEL", "KNOW_ETS", 
                              "KNOW_SYMPTON_ETS", "CHECKUP_RULE_OUT_SYPHILIS", "CHECKUP_RULE_OUT_HIV",  "INTENDED_PREGNANCY", 
                              "PHYSICAL_VIOLENCE", "HOUSEHOLD_MEMBERS", "FIRST_PRENATAL_VISIT", "NUMBER_PRENATAL_VISITS", 
                              "PRENATAL_ATTENTION_PLACE", "COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE", "TOTAL_CHILDREN", 
                              "UNDER_SIXYEARS_CHILDREN", "HEALTH_INSURANCE")


data2 <- data1 %>% mutate(WEALTH_INDEX = case_when(WEALTH_INDEX == 'MIDDLE' ~ 'MIDDLE',
                                           WEALTH_INDEX == 'POOR' ~ 'POOR',
                                           WEALTH_INDEX == 'POOREST' ~ 'POOREST',
                                           WEALTH_INDEX == 'RICH' ~ 'RICH-RICHEST',
                                           WEALTH_INDEX == 'RICHEST' ~ 'RICH-RICHEST'),
                 
                 RELATIONSHIP_HOUSEHOLD_HEAD = case_when(RELATIONSHIP_HOUSEHOLD_HEAD == 'DAUGTHER/SON' ~ 'DAUGTHER/SON',
                                                         RELATIONSHIP_HOUSEHOLD_HEAD == 'HEAD' ~ 'HEAD-OTHER',
                                                         RELATIONSHIP_HOUSEHOLD_HEAD == 'OTHER' ~ 'HEAD-OTHER',
                                                         RELATIONSHIP_HOUSEHOLD_HEAD == 'WIFE' ~ 'WIFE'),
                 ETHNICITY = case_when(ETHNICITY == 'SPANISH' ~ 'SPANISH',
                                       ETHNICITY != 'SPANISH' ~ 'NO_SPANISH'),
                 
                 EDU_LEVEL = case_when(EDU_LEVEL == 'HIGHER' ~ 'HIGHER',
                                       EDU_LEVEL == 'NONE/PRESCHOOL' ~ 'NONE/PRESCHOOL-PRIMARY',
                                       EDU_LEVEL == 'PRIMARY' ~ 'NONE/PRESCHOOL-PRIMARY',
                                       EDU_LEVEL == 'SECONDARY' ~ 'SECONDARY'),
                 
                 PARTNER_EDU_LEVEL = case_when(PARTNER_EDU_LEVEL == 'HIGHER' ~ 'HIGHER',
                                               PARTNER_EDU_LEVEL == 'NONE/PRESCHOOL' ~ 'NONE/PRESCHOOL-PRIMARY',
                                               PARTNER_EDU_LEVEL == 'PRIMARY' ~ 'NONE/PRESCHOOL-PRIMARY',
                                               PARTNER_EDU_LEVEL == 'SECONDARY' ~ 'SECONDARY'),
                 
                 PRENATAL_ATTENTION_PLACE = case_when(PRENATAL_ATTENTION_PLACE == 'MINSA' ~ 'MINSA',
                                                      PRENATAL_ATTENTION_PLACE != 'MINSA' ~ 'NO_MINSA'))

# GET DUMMIES

dummy <- caret::dummyVars(~ ., data = data2, fullRank = TRUE)

dummy
#> Dummy Variable Object
#> 
#> Formula: ~.
#> 2 variables, 1 factors
#> Variables and levels will be separated by '.'
#> A full rank encoding is used

predict(dummy, data2)

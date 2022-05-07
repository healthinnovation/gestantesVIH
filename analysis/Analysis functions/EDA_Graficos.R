library(tidyverse)
library(scales)
library(ggplot2)
data_read<-read.csv("./data/datafinal.csv")
head(data_read)
names(data_read)

data <- data_read %>% select( "year", "AGE_MOTHER", "WEALTH_INDEX", "RELATIONSHIP_HOUSEHOLD_HEAD", "TYPE_PLACE_RESIDENCE",                       
                      "ETHNICITY", "DEPARTAMEN", "LITERACY", "CURRENT_MARITAL_STATUS",  "EDU_LEVEL", "NATURAL_REGION", 
                      "PARTNER_EDU_LEVEL", "CAN_SOMETHING_BE_DONE_PREVENT_AIDS", "KNOW_ETS", "CHECKUP_RULE_OUT_SYPHILIS",
                      "CHECKUP_RULE_OUT_HIV", "PRENATAL_CARE_ATTENTION",  "INTENDED_PREGNANCY", "PHYSICAL_VIOLENCE",
                      "DIAGNOSTED_STD_LAST_12_MONTHS", "KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD", "HOUSEHOLD_MEMBERS",                         
                      "FIRST_PRENATAL_VISIT", "NUMBER_PRENATAL_VISITS", "PRENATAL_ATTENTION_PLACE", 
                      "COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE", "HAVE_ITS_SYMPTOMS",  "TOTAL_CHILDREN", 
                      "UNDER_SIXYEARS_CHILDREN", "HEALTH_INSURANCE", "LAST_BIRTH")

#variable respuesta : CHECKUP_RULE_OUT_HIV  

str(data)
summary(data)

##################################
# ANALISIS UNIVARIADO: NUMÉRICAS
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
# ANALISIS UNIVARIADO: CATEGÓRICAS
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
  ggtitle("Proporciòn de Edad de la madre por periodo")

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
  ggtitle("Proporciòn de Indice de riqueza por periodo")

library(tidymodels)
library(tidyverse)
library(caret)

data_read <- read.csv("./data/datatest.csv")
glimpse(data_read)

data <- data_read %>% select(-c(CASEID,V001,V022, V005, IDX94, B2, B3, filtro))
glimpse(data)

table(data$year)*100/nrow(data)
table(data$CHECKUP_RULE_OUT_HIV)*100/nrow(data)

sum(is.na(data$CHECKUP_RULE_OUT_HIV)) *100/nrow(data)

df <- data %>% filter(year != ('2019') & !is.na(CHECKUP_RULE_OUT_HIV))

table(df$year)
sum(is.na(df$CHECKUP_RULE_OUT_HIV)) *100/nrow(df)
sum(is.na(df$CHECKUP_RULE_OUT_SYPHILIS)) *100/nrow(df)

#----- Valores Nulos
round(colSums(is.na(df))*100/nrow(df),2)
names(df[colSums(is.na(df))*100/nrow(df)>0])

# AGE MOTHER: ok
#-------------
plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de Edad de la madre por periodo")

# WEALTH_INDEX: ok
#--------------
plot_data <- group_by(df, year) %>%
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

# RELATIONSHIP_HOUSEHOLD_HEAD: ok
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de relaci贸n con la cabeza del hogar por periodo")

# TYPE_PLACE_RESIDENCE: ok
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de tipo de lugar de residencia por periodo")

# ETHNICITY: no considerar o (spanish,no spanish)
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de etnicidad por periodo")

# LITERACY: no o abletoread,others
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de alfabetizaci贸n por periodo")

# CURRENT_MARITAL_STATUS: NO o married/livingtogether , otros
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de estado civil por periodo")

# EDU_LEVEL: none/preschool-primary, secondary, higher
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de nivel educativo por periodo")

# NATURAL_REGION: ok
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de regiones naturales por periodo")

# PARTNER_APPROVE_DISAPPROVE_FAMILY_PLANNING: NO - appprove, otros 
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de opini贸n de planificaci贸n familiar de la pareja por periodo")

# PARTNER_EDU_LEVEL: none/preschool-primary, secondary, higher
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de nivel educativo de la pareja por periodo")

# CAN_SOMETHING_BE_DONE_PREVENT_AIDS: NO
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de prevenci贸n de SIDA por periodo")

# KNOW_ETS: OK
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de conocimiento de ETS por periodo")

# KNOW_SYMPTON_ETS: OK
#------------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de conocimiento de s铆ntomas de ETS por periodo")

# PRENATAL_CARE_ATTENTION: NO
#-----------------------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de atenci贸n de cuidado prenatal por periodo")

# INTENDED_PREGNANCY: OK
#------------------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de embarazo intencionado por periodo")

# PHYSICAL_VIOLENCE: NO
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de violencia f铆sica por periodo")

# DIAGNOSTED_STD_LAST_12_MONTHS: NO
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de diagn贸stico de ETS durante los 煤ltimos 12 meses por periodo")

# KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD: OK
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de conocimiento de tramisi贸n de VIH de madre a hijo por periodo")


# HOUSEHOLD_MEMBERS: OK
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de n煤mero de miembros del hogar por periodo")

# PRENATAL_ATTENTION_PLACE: MINSA , NO MINSA
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de lugares de atenci贸n prenatal por periodo")

# COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE: OK
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de lugares de atenci贸n prenatal por periodo")

# HAVE_ITS_SYMPTOMS: none , know.min.1
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de mujeres con s铆ntomas de ETS por periodo")

# HEALTH_INSURANCE: OK
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de afiliaci贸n a seguros de salud por periodo")

# LAST_BIRTH: OK
#--------------

plot_data <- group_by(df, year) %>%
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
  ggtitle("Proporci贸n de hijos de menos de 12 meses")


# Target cambio
#----------------
table(df$CHECKUP_RULE_OUT_HIV)
df$CHECKUP_RULE_OUT_HIV <- ifelse(df$CHECKUP_RULE_OUT_HIV=='NO',1,0)
table(df$CHECKUP_RULE_OUT_HIV)

# VARIABLES NUMERICAS
#----------------------
table(df$FIRST_PRENATAL_VISIT)
table(df$NUMBER_PRENATAL_VISITS)
table(df$TOTAL_CHILDREN)
table(df$year,df$UNDER_SIXYEARS_CHILDREN)

#- FIRST PRENATAL VISIT: 0-1, 2, 3, 4, 5 a m醩
t1 <- group_by(df, year)%>%
  mutate(n = n())%>%
  group_by(year, FIRST_PRENATAL_VISIT)%>%
  summarise(conteo = sum(CHECKUP_RULE_OUT_HIV)/n())
t1

ggplot(t1, aes(x = year, y = conteo, group = as.factor(FIRST_PRENATAL_VISIT), color = as.factor(FIRST_PRENATAL_VISIT)))+
  geom_line(size = 1)+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top"
    )
  
# NUMBER PRENATAL VISIT: 
# grupo 1: 1 a 7 =8k
# grupo 2: 8 a 10 = 13k
# grupo 3: 11 a m醩 = 8k
t2 <- group_by(df, year)%>%
  mutate(n = n())%>%
  group_by(year, NUMBER_PRENATAL_VISITS)%>%
  summarise(conteo = sum(CHECKUP_RULE_OUT_HIV)/n())
t2

ggplot(t2, aes(x = year, y = conteo, group = as.factor(NUMBER_PRENATAL_VISITS), color = as.factor(NUMBER_PRENATAL_VISITS)))+
  geom_line(size = 1)+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top"
  )

#TOTAL CHILDREN : 1, 2, 3, 4, 5 a mas
t3 <- group_by(df, year)%>%
    mutate(n = n())%>%
    group_by(year, TOTAL_CHILDREN)%>%
    summarise(conteo = sum(CHECKUP_RULE_OUT_HIV)/n())
t3
  
ggplot(t3, aes(x = year, y = conteo, group = as.factor(TOTAL_CHILDREN), color = as.factor(TOTAL_CHILDREN)))+
geom_line(size = 1)+
theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top"
  )

#UNDER_SIXYEARS_CHILDREN: 1 , 2 a m醩
t4 <- group_by(df, year)%>%
  mutate(n = n())%>%
  group_by(year, UNDER_SIXYEARS_CHILDREN)%>%
  summarise(conteo = sum(CHECKUP_RULE_OUT_HIV)/n())
t4

ggplot(t4, aes(x = year, y = conteo, group = as.factor(UNDER_SIXYEARS_CHILDREN), color = as.factor(UNDER_SIXYEARS_CHILDREN)))+
  geom_line(size = 1)+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top"
  )
table(df$UNDER_SIXYEARS_CHILDREN)*100/nrow(df)

#-----------------
#RECATEGORIZACION
#-----------------

df1 <- df %>% select( "year","DEPARTAMEN", 
                          "AGE_MOTHER", "WEALTH_INDEX", "RELATIONSHIP_HOUSEHOLD_HEAD", "TYPE_PLACE_RESIDENCE",                       
                          "EDU_LEVEL", "NATURAL_REGION", "PARTNER_EDU_LEVEL", "KNOW_ETS", "KNOW_SYMPTON_ETS",  "INTENDED_PREGNANCY", 
                          "PHYSICAL_VIOLENCE",#NO
                          "KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD","HOUSEHOLD_MEMBERS", "PRENATAL_ATTENTION_PLACE", 
                          "COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE", "HAVE_ITS_SYMPTOMS",#none know_min1
                          "HEALTH_INSURANCE", "LAST_BIRTH", "FIRST_PRENATAL_VISIT", "NUMBER_PRENATAL_VISITS", 
                          "TOTAL_CHILDREN", "UNDER_SIXYEARS_CHILDREN", "CHECKUP_RULE_OUT_HIV")

df2 <- df1 %>% mutate( EDU_LEVEL = case_when(EDU_LEVEL == 'HIGHER' ~ 'HIGHER',
                       EDU_LEVEL == 'NONE/PRESCHOOL' ~ 'NONE/PRESCHOOL-PRIMARY',
                       EDU_LEVEL == 'PRIMARY' ~ 'NONE/PRESCHOOL-PRIMARY',
                       EDU_LEVEL == 'SECONDARY' ~ 'SECONDARY'),
                          
                          PARTNER_EDU_LEVEL = case_when(PARTNER_EDU_LEVEL == 'HIGHER' ~ 'HIGHER',
                                                        PARTNER_EDU_LEVEL == 'NONE/PRESCHOOL' ~ 'NONE/PRESCHOOL-PRIMARY',
                                                        PARTNER_EDU_LEVEL == 'PRIMARY' ~ 'NONE/PRESCHOOL-PRIMARY',
                                                        PARTNER_EDU_LEVEL == 'SECONDARY' ~ 'SECONDARY'),
                          
                          PRENATAL_ATTENTION_PLACE = case_when(PRENATAL_ATTENTION_PLACE == 'MINSA' ~ 'MINSA',
                                                               PRENATAL_ATTENTION_PLACE != 'MINSA' ~ 'NO_MINSA'),
                          
                          HAVE_ITS_SYMPTOMS = case_when(HAVE_ITS_SYMPTOMS == 'NONE' ~ 'NONE',
                                                        HAVE_ITS_SYMPTOMS == 'BOTH' ~ 'MIN_1',
                                                        HAVE_ITS_SYMPTOMS == 'ONLY FLOW' ~ 'MIN_1',
                                                        HAVE_ITS_SYMPTOMS == 'ONLY SORE/ULCER' ~ 'MIN_1'))

table(df1$HAVE_ITS_SYMPTOMS)
table(df2$HAVE_ITS_SYMPTOMS)

glimpse(df2)
table(df2$CHECKUP_RULE_OUT_HIV)
sum(is.na(df2$CHECKUP_RULE_OUT_HIV))

df2$CHECKUP_RULE_OUT_HIV <- as.factor(df2$CHECKUP_RULE_OUT_HIV)
table(df2$year)

#-------------------
#TRAIN Y TEST, 2021
#------------------

df3 <- df2 %>% filter(year != ('2021'))
glimpse(df3)

IVO <- 25 #VIH (SIPHILIS ->17)
df3[,IVO] <- as.factor(df3[,IVO])
a駉s <- unique(df3[,1])
depa <- unique(df3[,2])
#n <- NULL
datos_train.B <- df3[FALSE,ncol(df3)]
datos_test.B <-  df3[FALSE,ncol(df3)]
set.seed(2020)
for(d in 1:length(depa)){
  for(a in 1:length(a駉s)){
    dBASE <- df3 %>% filter(year==a駉s[a],DEPARTAMEN==depa[d])
    train.B <- createDataPartition(y = dBASE[,IVO], p = 0.8, list = FALSE, times = 1)
    #n[a] <- length(train.B)
    datos_train.B <- rbind(datos_train.B,dBASE[train.B, ])
    datos_test.B  <- rbind(datos_test.B,dBASE[-train.B, ])
  }
  
}

head(datos_train.B)
head(datos_test.B)

#BIND TRAIN
#datos_train.B <- datos_train.B%>%
#  mutate(case="TRAIN")
#datos_test.B <- datos_test.B%>%
#  mutate(case="TEST")
#gestantes <- bind_rows(datos_train.B,datos_test.B)

#data 2021
df2021 <- df2%>%
  filter(year==2021)

###########################
# MODELO: DECISIOM TREE
###########################

df_train <- datos_train.B %>% select(-c(1,2))
glimpse(df_train)
df_test <- datos_test.B %>% select(-c(1,2))
glimpse(df_test)

df_train$CHECKUP_RULE_OUT_HIV <- as.factor(df_train$CHECKUP_RULE_OUT_HIV)
df_test$CHECKUP_RULE_OUT_HIV <- as.factor(df_test$CHECKUP_RULE_OUT_HIV)
#-------------------------------------

set.seed(123)
trees_folds <- vfold_cv(df_train, v = 5, strata = CHECKUP_RULE_OUT_HIV)
trees_folds

trees_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

trees_rec <- recipe(CHECKUP_RULE_OUT_HIV~., data = df_train)

trees_wf <- workflow() %>%
  add_model(trees_spec) %>%
  add_recipe(trees_rec)

library(doParallel)
library(parallel)
registerDoParallel()

set.seed(123)
trees_res <- tune_grid(
  trees_wf,
  resamples = trees_folds,
  grid = 20,
  metrics = metric_set(mn_log_loss,accuracy,roc_auc,specificity,sensitivity,j_index)
)

trees_res

trees_res %>%
  collect_metrics() %>%
  filter(.metric == "j_index") %>%
  select(mean, min_n, tree_depth, cost_complexity) %>%
  pivot_longer(min_n:cost_complexity,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "J_Index")

trees_grid <- grid_regular(
  min_n(range = c(15,30)),
  tree_depth(range = c(4, 10)),
  cost_complexity(range = c(-8,-3),trans = log10_trans()) ,
  levels = 5
)
trees_grid

log10(0.001)
log10(0.000001)

min(trees_grid$min_n); max(trees_grid$min_n)
min(trees_grid$tree_depth); max(trees_grid$tree_depth)
min(trees_grid$cost_complexity); max(trees_grid$cost_complexity)

set.seed(123)
regular_res <- tune_grid(
  trees_wf,
  resamples = trees_folds,
  grid = trees_grid,
  metrics = metric_set(mn_log_loss,accuracy,roc_auc,specificity,sensitivity,j_index)
)

regular_res

show_best(regular_res,"sensitivity")
show_best(regular_res,"specificity")
show_best(regular_res,"j_index")

best_auc <- select_best(regular_res, "j_index")

final_trees <- finalize_model(
  trees_spec,
  best_auc
)

final_trees

library(vip)

final_trees %>%
  fit(CHECKUP_RULE_OUT_HIV ~ .,
      data = df_train
  ) %>%
  vip(geom = "point")

final_wf <- workflow() %>%
  add_recipe(trees_rec) %>%
  add_model(final_trees)

#Fit train
final_fit <- final_wf %>% fit(data = df_train)
final_fit

#Predict test
df_test_wt <- glimpse(df_test) %>% select(-CHECKUP_RULE_OUT_HIV)

predictions <- final_fit %>%
  predict(new_data = df_test_wt, type = "prob")
head(predictions)

predictions_class <- final_fit %>%
  predict(new_data = df_test_wt)
head(predictions_class)

library(caret)
confusionMatrix(predictions_class$.pred_class,as.factor(df_test$CHECKUP_RULE_OUT_HIV),positive = '1')
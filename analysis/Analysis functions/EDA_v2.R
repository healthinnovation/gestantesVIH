library(tidymodels)
library(tidyverse)

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

#----- Recategorizacion
lista_var <- glimpse(df)

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
  ggtitle("Proporción de Edad de la madre por periodo")

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
  ggtitle("Proporción de relación con la cabeza del hogar por periodo")

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
  ggtitle("Proporción de tipo de lugar de residencia por periodo")

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
  ggtitle("Proporción de etnicidad por periodo")

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
  ggtitle("Proporción de alfabetización por periodo")

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
  ggtitle("Proporción de estado civil por periodo")

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
  ggtitle("Proporción de nivel educativo por periodo")

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
  ggtitle("Proporción de regiones naturales por periodo")

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
  ggtitle("Proporción de opinión de planificación familiar de la pareja por periodo")

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
  ggtitle("Proporción de nivel educativo de la pareja por periodo")

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
  ggtitle("Proporción de prevención de SIDA por periodo")

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
  ggtitle("Proporción de conocimiento de ETS por periodo")

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
  ggtitle("Proporción de conocimiento de síntomas de ETS por periodo")

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
  ggtitle("Proporción de atención de cuidado prenatal por periodo")

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
  ggtitle("Proporción de embarazo intencionado por periodo")

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
  ggtitle("Proporción de violencia física por periodo")

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
  ggtitle("Proporción de diagnóstico de ETS durante los últimos 12 meses por periodo")

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
  ggtitle("Proporción de conocimiento de tramisión de VIH de madre a hijo por periodo")


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
  ggtitle("Proporción de número de miembros del hogar por periodo")

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
  ggtitle("Proporción de lugares de atención prenatal por periodo")

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
  ggtitle("Proporción de lugares de atención prenatal por periodo")

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
  ggtitle("Proporción de mujeres con síntomas de ETS por periodo")

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
  ggtitle("Proporción de afiliación a seguros de salud por periodo")

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
  ggtitle("Proporción de hijos de menos de 12 meses")

# VARIABLES NUMERICAS
#----------------------
table(df$FIRST_PRENATAL_VISIT)
table(df$NUMBER_PRENATAL_VISITS)
table(df$TOTAL_CHILDREN)
table(df$UNDER_SIXYEARS_CHILDREN)

library(tidyverse)

#https://fhernanb.github.io/libro_regresion/multicoli.html
#https://www.statology.org/variance-inflation-factor-r/

data_read<-read.csv("./data/data2021final.csv")
head(data_read)
names(data_read)

data0 <- data_read %>% select( "year", "AGE_MOTHER", "WEALTH_INDEX", "RELATIONSHIP_HOUSEHOLD_HEAD", "TYPE_PLACE_RESIDENCE",                       
                                  "ETHNICITY", "EDU_LEVEL", "NATURAL_REGION", "PARTNER_EDU_LEVEL", "KNOW_ETS", 
                                  "KNOW_SYMPTON_ETS", "CHECKUP_RULE_OUT_HIV",  "INTENDED_PREGNANCY", 
                                  "PHYSICAL_VIOLENCE", "HOUSEHOLD_MEMBERS", "FIRST_PRENATAL_VISIT", "NUMBER_PRENATAL_VISITS", 
                                  "PRENATAL_ATTENTION_PLACE", "COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE", "TOTAL_CHILDREN", 
                                  "UNDER_SIXYEARS_CHILDREN", "HEALTH_INSURANCE")

#variable respuesta : CHECKUP_RULE_OUT_HIV  
summary(data0)

data1 <- data0 %>% filter(!is.na(CHECKUP_RULE_OUT_HIV))
sum(is.na(data1$CHECKUP_RULE_OUT_HIV))*100/nrow(data1)

#----------------------- Tipo de datos
glimpse(data1)

#---------------------- Agrupacion de variables

data <- data1 %>% mutate(CHECKUP_RULE_OUT_HIV = case_when(CHECKUP_RULE_OUT_HIV == 'YES' ~ 0,
                                                           CHECKUP_RULE_OUT_HIV == 'NO' ~ 1) ,
                          
                          ETHNICITY = case_when(ETHNICITY == 'SPANISH' ~ 'SPANISH',
                                                ETHNICITY == 'FOREIGNER' ~ 'FOREIGNER',
                                                ETHNICITY == 'AIMARA' ~ 'INDIGENOUS_LANGUAGES',
                                                ETHNICITY == 'OTHER INDIGENOUS' ~ 'INDIGENOUS_LANGUAGES',
                                                ETHNICITY == 'QUECHUA' ~ 'INDIGENOUS_LANGUAGES'),
                          
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





#----------------------- Valores Nulos
map_dbl(data, .f = function(x){round(sum(is.na(x)*100/length(x)),2)})
names(data[map_dbl(data, .f = function(x){round(sum(is.na(x)*100/length(x)),2)})>0])


########################################
#-------------- Imputacion ------------
########################################

##### Variables categoricas########

#PARTNER_EDU_LEVEL
table(data$PARTNER_EDU_LEVEL)*100/nrow(data)
data$PARTNER_EDU_LEVEL[is.na(data$PARTNER_EDU_LEVEL)] <- 'SECONDARY'

#KNOW_SYMPTON_ETS
table(data$KNOW_SYMPTON_ETS)*100/nrow(data)
data$KNOW_SYMPTON_ETS[is.na(data$KNOW_SYMPTON_ETS)] <- 'NO'

#PHYSICAL_VIOLENCE  
table(data$PHYSICAL_VIOLENCE)*100/nrow(data)
data$PHYSICAL_VIOLENCE[is.na(data$PHYSICAL_VIOLENCE)] <- 'NO'

##### VARIABLES NUMERICAS###########

#FIRST_PRENATAL_VISIT 
boxplot(data$FIRST_PRENATAL_VISIT)
data$FIRST_PRENATAL_VISIT[is.na(data$FIRST_PRENATAL_VISIT)] <- mean(data$FIRST_PRENATAL_VISIT,na.rm = TRUE)

#NUMBER_PRENATAL_VISITS
boxplot(data$NUMBER_PRENATAL_VISITS)
data$NUMBER_PRENATAL_VISITS[is.na(data$NUMBER_PRENATAL_VISITS)] <- mean(data$NUMBER_PRENATAL_VISITS,na.rm = TRUE)


########################################
#-------------- Get Dummies ------------
########################################
glimpse(data)

dummy <- caret::dummyVars(~ ., data = data, fullRank = TRUE, sep = '.')
dummy

data_dummy <- as.data.frame(predict(dummy, data))
head(data_dummy)

table(data_dummy$year)

######################################
#--------------Correlacion------------
######################################
glimpse(data_dummy)

library(DataExplorer)
plot_correlation(
  data = data,
  type = "discrete",
  title = "Matriz de correlacion",
  theme_config = list(legend.position = "none",
                      plot.title = element_text(size = 16, face = "bold"),
                      axis.title = element_blank(),
                      axis.text.x = element_text(angle = -45, hjust = +0.1)
  )
)

matriz_cor <- data_dummy %>% select(-c(year,CHECKUP_RULE_OUT_HIV)) %>% cor()# %>% round(digits=2)
head(matriz_cor)

##############################################
#------------ Variable Respuesta ------------
##############################################
data$CHECKUP_RULE_OUT_HIV <- as.factor(data$CHECKUP_RULE_OUT_HIV)

ggplot(data = data, aes(x = CHECKUP_RULE_OUT_HIV , y = ..count.., fill = CHECKUP_RULE_OUT_HIV )) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "CHECKUP_RULE_OUT_HIV ") +
  theme_bw() +
  theme(legend.position = "bottom")

prop.table(table(data$CHECKUP_RULE_OUT_HIV)) %>% round(digits = 2)

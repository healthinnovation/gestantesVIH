library(tidyverse)
data_read<-read.csv("./data/data_analysis.csv")
head(data_read)
names(data_read)

#https://fhernanb.github.io/libro_regresion/multicoli.html
#https://www.statology.org/variance-inflation-factor-r/

data <- data_read %>% select( "CASEID","year","IDX94","V005"
                  ,"AGE_MOTHER"
                  ,"WEALTH_INDEX"                              
                  ,"RELATIONSHIP_HOUSEHOLD_HEAD"               
                  ,"TYPE_PLACE_RESIDENCE"                      
                  ,"ETHNICITY"                                 
                  ,"DEPARTAMEN"                                
                  ,"LITERACY"                                  
                  ,"CURRENT_MARITAL_STATUS"                    
                  ,"EDU_LEVEL"                                 
                  ,"NATURAL_REGION"                            
                  ,"PARTNER_APPROVE_DISAPPROVE_FAMILY_PLANNING"
                  ,"PARTNER_EDU_LEVEL"                         
                  ,"CAN_SOMETHING_BE_DONE_PREVENT_AIDS"        
                  ,"KNOW_ETS"                                  
                  ,"KNOW_SYMPTON_ETS"                          
                  ,"CHECKUP_RULE_OUT_SYPHILIS"                 
                  ,"CHECKUP_RULE_OUT_HIV"                      
                  ,"PRENATAL_CARE_ATTENTION"                   
                  ,"INTENDED_PREGNANCY"                        
                  ,"PHYSICAL_VIOLENCE"                         
                  ,"DIAGNOSTED_STD_LAST_12_MONTHS"             
                  ,"KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD"     
                  ,"HOUSEHOLD_MEMBERS"                         
                  ,"FIRST_PRENATAL_VISIT"                      
                  ,"NUMBER_PRENATAL_VISITS"                    
                  ,"PRENATAL_ATTENTION_PLACE"                  
                  ,"COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE"    
                  ,"HAVE_ITS_SYMPTOMS"                         
                  ,"TOTAL_CHILDREN"                            
                  ,"UNDER_SIXYEARS_CHILDREN"                   
                  ,"HEALTH_INSURANCE"       
                  ,"LAST_BIRTH")

############################################################
# ----------- Análisis exploratorio de datos ---------------
############################################################

# Filtrando mujeres que tienen un hijo en el último año y primer hijo nacido en en el último año
data <- data %>% filter(LAST_BIRTH=='LESS THAN 12 MONTHS' & IDX94==1) %>% mutate( ID = paste(CASEID,year,sep = "_"))
#write.csv(data,'./data/datafinal.csv')

#--- ID unicos
length(data$ID)
length(unique(data$ID))

#----------------------- Tipo de datos
glimpse(data)
#variable respuesta : CHECKUP_RULE_OUT_HIV  

#----------------------- Valores Nulos
map_dbl(data, .f = function(x){round(sum(is.na(x)*100/length(x)),2)})
names(data[map_dbl(data, .f = function(x){round(sum(is.na(x)*100/length(x)),2)})>0])


########################################
#-------------- Imputacion ------------
########################################

##### Variables categoricas########
#Ethnicity
table(data$ETHNICITY)
data$ETHNICITY[is.na(data$ETHNICITY)] <- 'QUECHUA'

#CURRENT_MARITAL_STATUS
table(data$CURRENT_MARITAL_STATUS)
data$CURRENT_MARITAL_STATUS[is.na(data$CURRENT_MARITAL_STATUS)] <- 'MARRIED/LIVING_TOGETHER'

#PARTNER_APPROVE_DISAPPROVE_FAMILY_PLANNING
table(data$PARTNER_APPROVE_DISAPPROVE_FAMILY_PLANNING)
data$PARTNER_APPROVE_DISAPPROVE_FAMILY_PLANNING[is.na(data$PARTNER_APPROVE_DISAPPROVE_FAMILY_PLANNING)] <- 'APPROVE'

#PARTNER_EDU_LEVEL
table(data$PARTNER_EDU_LEVEL)
data$PARTNER_EDU_LEVEL[is.na(data$PARTNER_EDU_LEVEL)] <- 'SECONDARY'

#CAN_SOMETHING_BE_DONE_PREVENT_AIDS   
table(data$CAN_SOMETHING_BE_DONE_PREVENT_AIDS)
data$CAN_SOMETHING_BE_DONE_PREVENT_AIDS[is.na(data$CAN_SOMETHING_BE_DONE_PREVENT_AIDS)] <- 'YES'

#KNOW_ETS         
table(data$KNOW_ETS)
data$KNOW_ETS[is.na(data$KNOW_ETS)] <- 'NONE'

#KNOW_SYMPTON_ETS
table(data$KNOW_SYMPTON_ETS)
data$KNOW_SYMPTON_ETS[is.na(data$KNOW_SYMPTON_ETS)] <- 'NO'

#CHECKUP_RULE_OUT_SYPHILIS 
table(data$CHECKUP_RULE_OUT_SYPHILIS)
data$CHECKUP_RULE_OUT_SYPHILIS[is.na(data$CHECKUP_RULE_OUT_SYPHILIS)] <- 'YES'

#CHECKUP_RULE_OUT_HIV 
table(data$CHECKUP_RULE_OUT_HIV)
data$CHECKUP_RULE_OUT_HIV[is.na(data$CHECKUP_RULE_OUT_HIV)] <- 'YES'

#INTENDED_PREGNANCY
table(data$INTENDED_PREGNANCY)
data$INTENDED_PREGNANCY[is.na(data$INTENDED_PREGNANCY)] <- 'ENTONCES'

#PHYSICAL_VIOLENCE  
table(data$PHYSICAL_VIOLENCE)
data$PHYSICAL_VIOLENCE[is.na(data$PHYSICAL_VIOLENCE)] <- 'YES'

#DIAGNOSTED_STD_LAST_12_MONTHS 
table(data$DIAGNOSTED_STD_LAST_12_MONTHS)
data$DIAGNOSTED_STD_LAST_12_MONTHS[is.na(data$DIAGNOSTED_STD_LAST_12_MONTHS)] <- 'NO'

#KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD
table(data$KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD)
data$KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD[is.na(data$KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD)] <- 'NO'

#PRENATAL_ATTENTION_PLACE
table(data$PRENATAL_ATTENTION_PLACE)
data$PRENATAL_ATTENTION_PLACE[is.na(data$PRENATAL_ATTENTION_PLACE)] <- 'MINSA'  # FF.AA. dentro de others
  
#COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE
table(data$COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE)
data$COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE[is.na(data$COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE)] <- 'ENTONCES'

#HAVE_ITS_SYMPTOMS  
table(data$HAVE_ITS_SYMPTOMS)
data$HAVE_ITS_SYMPTOMS[is.na(data$HAVE_ITS_SYMPTOMS)] <- 'NONE'

#HEALTH_INSURANCE
table(data$HEALTH_INSURANCE)
data$HEALTH_INSURANCE[is.na(data$HEALTH_INSURANCE)] <- 'SIS'

##### VARIABLES NUMERICAS###########

#FIRST_PRENATAL_VISIT 
boxplot(data$FIRST_PRENATAL_VISIT)
data$FIRST_PRENATAL_VISIT[is.na(data$FIRST_PRENATAL_VISIT)] <- median(data$FIRST_PRENATAL_VISIT,na.rm = TRUE)

#NUMBER_PRENATAL_VISITS
boxplot(data$NUMBER_PRENATAL_VISITS)
data$NUMBER_PRENATAL_VISITS[is.na(data$NUMBER_PRENATAL_VISITS)] <- median(data$NUMBER_PRENATAL_VISITS,na.rm = TRUE)

##############################################
#------------ Variable Respuesta ------------
##############################################
ggplot(data = data, aes(x = CHECKUP_RULE_OUT_HIV , y = ..count.., fill = CHECKUP_RULE_OUT_HIV )) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "CHECKUP_RULE_OUT_HIV ") +
  theme_bw() +
  theme(legend.position = "bottom")

prop.table(table(data$CHECKUP_RULE_OUT_HIV)) %>% round(digits = 2)

########################################
#-------------- Get Dummies ------------
########################################

library(fastDummies)

######################################
#--------------Correlacion------------
######################################
data %>% cor() %>% round(digits=2)

library(DataExplorer)
plot_correlation(
  data = data,
  type = "continuous",
  title = "Matriz de correlación variables continuas",
  theme_config = list(legend.position = "none",
                      plot.title = element_text(size = 16, face = "bold"),
                      axis.title = element_blank(),
                      axis.text.x = element_text(angle = -45, hjust = +0.1)
  )
)

library(dplyr)

data2021 <- read.csv("data/data_analysis_2021.csv")
head(data2021)

table(data2021$year)

data = data2021 %>% filter(year %in% ('2021'))
table(data$year)

data1 <- data %>% select( "AGE_MOTHER", "WEALTH_INDEX", "RELATIONSHIP_HOUSEHOLD_HEAD", "TYPE_PLACE_RESIDENCE",                       
                          "ETHNICITY", "EDU_LEVEL", "NATURAL_REGION", "PARTNER_EDU_LEVEL", "KNOW_ETS", 
                          "KNOW_SYMPTON_ETS", "CHECKUP_RULE_OUT_HIV",  "INTENDED_PREGNANCY", 
                          "PHYSICAL_VIOLENCE", "HOUSEHOLD_MEMBERS", "FIRST_PRENATAL_VISIT", "NUMBER_PRENATAL_VISITS", 
                          "PRENATAL_ATTENTION_PLACE", "COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE", "TOTAL_CHILDREN", 
                          "UNDER_SIXYEARS_CHILDREN", "HEALTH_INSURANCE")

glimpse(data1)

table(data1$AGE_MOTHER)

data1$AGE_MOTHER = fct_recode(as.factor(data1$AGE_MOTHER), '15-24a'='15.24a', '25-30a'='25.30a', '30-35a'='30.35a', '35 a mas'='35.a.mas' )

data2 <- data1 %>% mutate(CHECKUP_RULE_OUT_HIV = case_when(CHECKUP_RULE_OUT_HIV == 'YES' ~ 0,
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

# GET DUMMIES

dummy <- caret::dummyVars(~ ., data = data2, fullRank = TRUE, sep = '.')
dummy

data_dummy <- as.data.frame(predict(dummy, data2))
head(data_dummy)

# Valores vacios en target
sum(is.na(data_dummy$CHECKUP_RULE_OUT_HIV))*100/nrow(data_dummy)

df2021 <- data_dummy %>% filter(!is.na(CHECKUP_RULE_OUT_HIV))
sum(is.na(df2021$CHECKUP_RULE_OUT_HIV))*100/nrow(df2021)

write.csv(df2021,"df2021.csv")

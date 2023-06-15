Data_preparation<-function(data){
  data %>% 
    select(
      CASEID,
      year,
      B2,
      V012,
      V190,
      V150,
      V025,
      V136,
      V131,
      V024,
      V001,
      V005,
      V022,
      HV115,
      S108N,
      SREGION,
      S815AA,
      S815AB,
      S815AC,
      S815AD,
      S815AE,
      S815AX,
      S816AA,
      S816AB,
      S816AC,
      S816AD,
      S816AE,
      S816AF,
      S816AG,
      S816AH,
      S816AI,
      S816AJ,
      S816AK,
      S816AL,
      S816AW,
      S411G,
      S411H,
      M2A,
      M2B,
      M2C,
      M2D,
      M2E,
      M2F,
      M2G,
      M2K,
      M2N,
      M10,
      M13,
      M14,
      M57E,
      M57F,
      M57G,
      M57H,
      M57I,
      M57J,
      M57K,
      M57L,
      M57M,
      M57N,
      M57O,
      M57P,
      M57Q,
      M57R,
      M57X,
      V763B,
      V763C,
      V774A,
      V774B,
      V774C,
      V481,
      IDX94,
    ) %>% 
    mutate(
      V005 = V005/1000000,
      
      AGE_MOTHER = ifelse(V012<20,"15-19 years",
                          ifelse(V012>=20&V012<25,"20-24 years",
                                 ifelse(V012>=25&V012<30,"25-29 years",
                                        ifelse(V012>=30&V012<35, "30-34 years",
                                              ifelse(V012>=35,"more than 35 years",NA))))),
      
      WEALTH_INDEX = factor(V190, levels = c(1:5), labels = c("POOREST","POOR","MIDDLE","RICH","RICHEST")),
      
      RELATIONSHIP_HOUSEHOLD_HEAD = ifelse(V150==1,"HEAD", 
                                           ifelse(V150==2, "WIFE", 
                                                  ifelse(V150==3, "DAUGTHER", 
                                                         ifelse(V150>=4&V012<=15, "OTHER",NA)))),
      
      TYPE_PLACE_RESIDENCE = ifelse(V025==1,"URBAN",
                                    ifelse(V025==2, "RURAL", NA)),
      
      HOUSEHOLD_MEMBERS = ifelse(V136<5,"1-4",
                                 ifelse(V136>=5&V136<7,"5-6",
                                        ifelse(V012>=7,"MORE THAN 6",NA))),
      
      ETHNICITY = ifelse(V131==1,"SPANISH",
                         ifelse(V131>=2&V131<=4, "QUECHUA/AIMARA/OTHER INDIGENOUS",NA)),
      
      DEPARTMENT = factor(V024, levels = c(1:25), labels = c("AMAZONAS","ANCASH","APURIMAC","AREQUIPA","AYACUCHO",
                                                             "CAJAMARCA","CALLAO","CUSCO","HUANCAVELICA","HUANUCO",
                                                             "ICA","JUNIN","LA LIBERTAD","LAMBAYEQUE","LIMA",
                                                             "LORETO","MADRE DE DIOS","MOQUEGUA","PASCO","PIURA",
                                                             "PUNO","SAN MARTIN","TACNA","TUMBES","UCAYALI")),
      
      CURRENT_MARITAL_STATUS = as.factor(ifelse(HV115==0,"SINGLE", 
                                      ifelse(HV115>=1&HV115<=2,"MARRIED/LIVING_TOGETHER", 
                                             ifelse(HV115>=3&HV115<=5,"WIDOWED/DIVORCED/SEPARATED", NA)))),
      
      EDUCATION_LEVEL = ifelse(S108N<=1,"NONE/PRESCHOOL/PRIMARY",
                               ifelse(S108N==2,"SECONDARY",
                                      ifelse(S108N>=3&S108N<6,"HIGHER",NA))),
      
      NATURAL_REGION = factor(SREGION, levels = c(1:4), labels = c("LIMA METROPOLITAN","REST OF COAST","HIGHLAND","JUNGLE")),
      
      
      KNOW_STD = (S815AA+S815AB+S815AC+S815AD+S815AE+S815AX), KNOW_STD=case_when(KNOW_STD==0~"NONE",
                                                                                 KNOW_STD==1~"AT LEAST 1",
                                                                                 KNOW_STD>=2&KNOW_STD<=6~"MORE THAN 1"),
      
      KNOW_STD_SYMPTOM = (S816AA+S816AB+S816AC+S816AD+S816AE+S816AF+S816AG+S816AH+S816AI+S816AJ+S816AK+S816AL+S816AW), KNOW_STD_SYMPTOM=case_when(KNOW_STD_SYMPTOM==0~"NO",
                                                                                                                                                  KNOW_STD_SYMPTOM==1~"AT LEAST 1",
                                                                                                                                                  KNOW_STD_SYMPTOM>=2~"MORE THAN 1"),
      
      CHECKUP_RULE_OUT_SYPHILIS = as.factor(ifelse(S411G == 0,"NO",
                                                   ifelse(S411G == 1,"YES",NA))),
      
      CHECKUP_RULE_OUT_HIV = as.factor(ifelse(S411H == 0,"NO",
                                              ifelse(S411H == 1,"YES",NA))),
      
      PRENATAL_ATTENTION_PROFESSIONAL_LEVEL = case_when((M2A==1) ~ "DOCTOR",
                                                        (M2B==1) ~ "NURSE",
                                                        (M2C==1) ~ "OBSTETRICIAN",
                                                        (M2D==1) ~ "TECHNICIAN",
                                                        (M2E==1 | M2F==1 | M2G==1 | M2K==1) ~ "OTHER",
                                                        (M2N==1) ~ "NOBODY"),

      
      INTENDED_PREGNANCY = as.factor(ifelse(M10 == 1, "YES, AT THAT TIME",
                                            ifelse(M10 == 2,"NO, WANTED TO WAIT LONGER",
                                                   ifelse(M10 == 3, "DID NOT WANT MORE CHILDREN", NA)))),
      
      FIRST_PRENATAL_VISIT = factor(M13, levels = c(0,1,2,3,4,5,6,7,8,9)),
      
      NUMBER_PRENATAL_VISITS = factor(M14, levels = c (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)),
      
      
      COMPLEXITY_PRENATAL_ATTENTION_PLACE = case_when((M57E==1 | M57I==1 | M57J==1) ~ "LEVEL 3",
                                                      (M57F==1 | M57H==1 | M57L==1 | M57M==1 | M57N==1 | M57Q==1) ~ "LEVEL 2",
                                                      (M57G==1 | M57K==1 | M57O==1 | M57P==1 | M57R==1 | M57X==1) ~ "LEVEL 1"),
                                                             
      HAVE_STD_SYMPTOM = case_when((V763B==1 & V763C==0)~"ONLY SORE/ULCER",
                                   (V763B==1 & V763C==8)~"ONLY SORE/ULCER",
                                   (V763B==0 & V763C==1)~"ONLY FLOW",
                                   (V763B==8 & V763C==1)~"ONLY FLOW",
                                   (V763B==1 & V763C==1)~"BOTH",
                                   ((V763B==0 & V763C==0) | (V763B==8 & V763C==8) | (V763B==8 & V763C==0) | (V763B==0 & V763C==8))~"NONE"),
      
      KNOW_MTCT_HIV_DURING_PREGNANCY = factor(V774A, levels = c(0,1,8), labels = c("NO","YES", NA)),
      
      KNOW_MTCT_HIV_DURING_CHILDBIRTH = factor(V774B, levels = c(0,1,8), labels = c("NO","YES", NA)),
      
      KNOW_MTCT_HIV_DURING_BREASTFEEDING = factor(V774C, levels = c(0,1,8), labels = c("NO","YES", NA)),
      
      KNOW_MTCT_HIV = (V774A+V774B+V774C), KNOWLEDGE_MTCT_HIV=case_when(KNOW_STD_SYMPTOM==0~"NO",
                                                                             KNOW_STD_SYMPTOM>=1&KNOW_STD_SYMPTOM<=3~"YES"),
      
      HEALTH_INSURANCE = ifelse(V481==0,"NO",
                                ifelse(V481==1,"YES", NA)),
    )    
}

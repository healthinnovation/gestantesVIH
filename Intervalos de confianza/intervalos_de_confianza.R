gestantes <- readr::read_csv("./datatest.csv")

library(dplyr)
library(survey)
library(srvyr)
library(tidyr)

variables <- c(
  "AGE_MOTHER"                            ,"WEALTH_INDEX"                         
  ,"RELATIONSHIP_HOUSEHOLD_HEAD"           ,"TYPE_PLACE_RESIDENCE"                 
  ,"HOUSEHOLD_MEMBERS"                     ,"ETHNICITY"                            
  ,"DEPARTMENT"                            ,"EDUCATION_LEVEL"                      
  ,"NATURAL_REGION"                        ,"KNOW_STD"                             
  ,"KNOW_STD_SYMPTOM"                      ,"CHECKUP_RULE_OUT_SYPHILIS"            
  ,"CHECKUP_RULE_OUT_HIV"                  ,"PRENATAL_ATTENTION_PROFESSIONAL_LEVEL"
  ,"INTENDED_PREGNANCY"                    ,"FIRST_PRENATAL_VISIT"                 
  ,"NUMBER_PRENATAL_VISITS"                ,"PRENATAL_ATTENTION_PLACE"             
  ,"HAVE_STD_SYMPTOM"                      ,"KNOW_MTCT_HIV_DURING_PREGNANCY"       
  ,"KNOW_MTCT_HIV_DURING_CHILDBIRTH"       ,"KNOW_MTCT_HIV_DURING_BREASTFEEDING"   
  ,"KNOW_MTCT_HIV"                         ,"HEALTH_INSURANCE"                     
  ,"CURRENT_MARITAL_STATUS" 
)

options(survey.lonely.psu = "remove")


ic <- expand_grid(variables, gestantes) |> 
  nest(data = -c(year, variables)) |> 
  mutate(
    design = purrr::map(
      data, 
      ~ as_survey_design(.x, ids = V001, strata = V022, weights = V005)
    ),
    ic = purrr::map2(
      design, variables, 
      ~ group_by(.x, across(all_of(.y))) |> 
        summarise(prop = survey_prop(vartype = "ci", proportion = TRUE))
    )
  ) 

ic_unnest <- ic |>
  select(-c(data, design)) |> 
  unnest(ic) 

view(ic_unnest)

write.csv(ic_unnest,file = "intervalos.csv")

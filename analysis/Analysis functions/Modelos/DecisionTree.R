library(tidyverse)
library(tidymodels)
#tidymodels::tidymodels_prefer()

train_read<-read.csv("./train.csv")
glimpse(train_read)

test_read<-read.csv("./test.csv")
glimpse(test_read)

train <- train_read %>% select (-c(1,2))
glimpse(train)
train$CHECKUP_RULE_OUT_HIV <- as.factor(train$CHECKUP_RULE_OUT_HIV)

test <- test_read %>% select (-c(1,2))
glimpse(test)
test$CHECKUP_RULE_OUT_HIV <- as.factor(test$CHECKUP_RULE_OUT_HIV)

#---------------------
set.seed(123)
tree_folds <- vfold_cv(train, v = 5, strata = CHECKUP_RULE_OUT_HIV)
tree_folds

tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_grid <- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 4)
tree_grid


library(doParallel)
no_cores <- detectCores() - 1  
cl<- makeCluster(no_cores)
registerDoParallel(cl)

set.seed(123)
tree_rs <- tune_grid(
  tree_spec,
  CHECKUP_RULE_OUT_HIV ~ .,
  resamples = tree_folds,
  grid = tree_grid,
  metrics = metric_set(mn_log_loss,accuracy,roc_auc,specificity,sensitivity,j_index)
)
tree_rs

#Evaluate model
collect_metrics(tree_rs)

autoplot(tree_rs) + theme_light(base_family = "IBMPlexSans")

#Best metric and model
show_best(tree_rs, "j_index")
show_best(tree_rs, "specificity")

select_best(tree_rs, "j_index")
"# A tibble: 1 x 4
  cost_complexity tree_depth min_n .config              
            <dbl>      <int> <int> <chr>                
1    0.0000000001         15    40 Preprocessor1_Model61"

#simpler_tree <- select_by_one_std_err(tree_rs,
#                                      -cost_complexity,
#                                      metric = "j_index")
#final_tree <- finalize_model(tree_spec, simpler_tree)

final_tree <- finalize_model(tree_spec, select_best(tree_rs, "j_index"))

final_fit <- fit(final_tree, CHECKUP_RULE_OUT_HIV ~ ., train)

# PREDICT WITH AUGMENT
#------------------------
test_df <- test %>% select(-CHECKUP_RULE_OUT_HIV)
predict <- augment(final_fit, test_df) 
glimpse(predict)

library(caret)
confusionMatrix(as.factor(predict$.pred_class),as.factor(test$CHECKUP_RULE_OUT_HIV),positive = '1')
"Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0 5019  649
         1  177  194
                                          
               Accuracy : 0.8632          
                 95% CI : (0.8543, 0.8718)
    No Information Rate : 0.8604          
    P-Value [Acc > NIR] : 0.271           
                                          
                  Kappa : 0.2561          
                                          
 Mcnemar's Test P-Value : <2e-16          
                                          
            Sensitivity : 0.23013         
            Specificity : 0.96594         
         Pos Pred Value : 0.52291         
         Neg Pred Value : 0.88550         
             Prevalence : 0.13959         
         Detection Rate : 0.03212         
   Detection Prevalence : 0.06143         
      Balanced Accuracy : 0.59803         
                                          
       'Positive' Class : 1          "

# PREDICT WITH THRESHOLD
#--------------------------
test_df <- test %>% select(-CHECKUP_RULE_OUT_HIV)

predictions <- final_fit %>%
  predict(new_data = test_df, type = "prob")
head(predictions)

tree_test_pred <- bind_cols(predictions, CHECKUP_RULE_OUT_HIV = test$CHECKUP_RULE_OUT_HIV)
tree_test_pred

#write.csv(tree_test_pred,file = "tree_test_pred.csv")

#Optimal threshold
library(probably)
threshold_data <- tree_test_pred %>%
  threshold_perf(CHECKUP_RULE_OUT_HIV, .pred_1, thresholds = seq(0.1, 0.5, by = 0.0025))

threshold_data %>%
  filter(.threshold %in% c(0.1,0.2, 0.3, 0.4))

#Rendimiento de la variable ´para encontrar el umbra óptimo
library(ggplot2)

threshold_data <- threshold_data %>%
  filter(.metric != "distance") %>%
  mutate(group = case_when(
    .metric == "sens" | .metric == "spec" ~ "1",
    TRUE ~ "2"
  ))

max_j_index_threshold <- threshold_data %>%
  filter(.metric == "j_index") %>%
  filter(.estimate == max(.estimate)) %>%
  pull(.threshold)
min(max_j_index_threshold)
max(max_j_index_threshold)

ggplot(threshold_data, aes(x = .threshold, y = .estimate, color = .metric, alpha = group)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_d(end = 0.9) +
  scale_alpha_manual(values = c(.4, 1), guide = "none") +
  geom_vline(xintercept = min(max_j_index_threshold), alpha = .6, color = "grey30") +#min(max_j_index_threshold)
  labs(
    x = "'Good' Threshold\n(above this value is considered 'good')",
    y = "Metric Estimate",
    title = "Balancing performance by varying the threshold",
    subtitle = "Sensitivity or specificity alone might not be enough!\nVertical line = Max J-Index"
  )

#Best umbral
best_umbral <- threshold_data %>%
  filter(.threshold == min(max_j_index_threshold))
"# A tibble: 3 x 5
  .threshold .metric .estimator .estimate group
       <dbl> <chr>   <chr>          <dbl> <chr>
1      0.488 sens    binary        0.0352 1    
2      0.488 spec    binary        0.765  1    
3      0.488 j_index binary       -0.200  2    "

predict_test <- ifelse(predictions$.pred_1>0.488,1,0)
head(predict_test)

library(caret)
confusionMatrix(as.factor(predict_test),as.factor(test$CHECKUP_RULE_OUT_HIV),positive = '1')
"Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0 5013  645
         1  183  198
                                         
               Accuracy : 0.8629         
                 95% CI : (0.854, 0.8715)
    No Information Rate : 0.8604         
    P-Value [Acc > NIR] : 0.2962         
                                         
                  Kappa : 0.2591         
                                         
 Mcnemar's Test P-Value : <2e-16         
                                         
            Sensitivity : 0.23488        
            Specificity : 0.96478        
         Pos Pred Value : 0.51969        
         Neg Pred Value : 0.88600        
             Prevalence : 0.13959        
         Detection Rate : 0.03279        
   Detection Prevalence : 0.06309        
      Balanced Accuracy : 0.59983        
                                         
       'Positive' Class : 1    "


# PREDICT 2021
#---------------

glimpse(df2021)

df2021 <- df2021 %>% select(-CHECKUP_RULE_OUT_HIV)

predictions2021 <- final_fit %>%
  predict(new_data = df2021 , type = "prob")
head(predictions)
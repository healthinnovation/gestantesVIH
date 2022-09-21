library(tidymodels)
library(tidyverse)

train <- read.csv("./DataFinal/Train-TestSeparado/train.csv")
test <- read.csv("./DataFinal/Train-TestSeparado/test.csv")
glimpse(train)
glimpse(test)

table(train$CHECKUP_RULE_OUT_HIV)*100/nrow(train)
table(test$CHECKUP_RULE_OUT_HIV)*100/nrow(test)

df_train <- train %>% select(-c(1,2))
glimpse(df_train)
df_test <- test %>% select(-c(1,2))
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
"# A tibble: 1 x 4
  cost_complexity tree_depth min_n .config               
            <dbl>      <int> <int> <chr>                 
1      0.00000001         10    18 Preprocessor1_Model022"

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
"Confusion Matrix and Statistics

          Reference
Prediction     0     1
         0 21806  2934
         1   696   943
                                          
               Accuracy : 0.8624          
                 95% CI : (0.8582, 0.8665)
    No Information Rate : 0.853           
    P-Value [Acc > NIR] : 7.697e-06       
                                          
                  Kappa : 0.2789          
                                          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.24323         
            Specificity : 0.96907         
         Pos Pred Value : 0.57535         
         Neg Pred Value : 0.88141         
             Prevalence : 0.14697         
         Detection Rate : 0.03575         
   Detection Prevalence : 0.06213         
      Balanced Accuracy : 0.60615         
                                          
       'Positive' Class : 1               
                                    "
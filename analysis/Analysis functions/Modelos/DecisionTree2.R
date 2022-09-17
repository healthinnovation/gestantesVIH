library(tidymodels)
library(tidyverse)

train <- read.csv("./DataFinal/Train-TestSeparado/train.csv")
test <- read.csv("./DataFinal/Train-TestSeparado/test.csv")
glimpse(train)
glimpse(test)


table(train$CHECKUP_RULE_OUT_HIV)*100/nrow(train)
table(test$CHECKUP_RULE_OUT_HIV)*100/nrow(test)
library(reshape)

df$CHECKUP_RULE_OUT_HIV <- as.factor(df$CHECKUP_RULE_OUT_HIV)
glimpse(df)

df$CHECKUP_RULE_OUT_HIV <- recode_factor(df$CHECKUP_RULE_OUT_HIV, '1' = '0', '0' = '1')
table(df$CHECKUP_RULE_OUT_HIV)*100/nrow(df)

#-------------------------------------

set.seed(123)
trees_split <- initial_split(df, strata = CHECKUP_RULE_OUT_HIV,prop = 0.80)
trees_train <- training(trees_split)
trees_test <- testing(trees_split)

set.seed(123)
trees_folds <- vfold_cv(trees_train, v = 5, strata = CHECKUP_RULE_OUT_HIV)
trees_folds

trees_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

trees_rec <- recipe(CHECKUP_RULE_OUT_HIV~., data = trees_train)

trees_wf <- workflow() %>%
  add_model(trees_spec) %>%
  add_recipe(trees_rec)

doParallel::registerDoParallel()

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
  min_n(range = c(20,40)),
  tree_depth(range = c(12, 16)),
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
      data = trees_train
  ) %>%
  vip(geom = "point")

final_wf <- workflow() %>%
  add_recipe(trees_rec) %>%
  add_model(final_trees)

final_res <- final_wf %>%
  last_fit(trees_split,
           metrics = metric_set(mn_log_loss,accuracy,roc_auc,specificity,sensitivity,j_index))

final_res %>%
  collect_metrics()

final_res$.predictions

# OPTIMO THRESHOLD
library(probably)
threshold_data <- final_res$.predictions[[1]] %>%
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


predict_test <- ifelse(final_res$.predictions[[1]]$.pred_1>0.488,1,0)
head(predict_test)

library(caret)
confusionMatrix(as.factor(predict_test),as.factor(final_res$.predictions[[1]]$CHECKUP_RULE_OUT_HIV),positive = '1')

library(tidyverse)
library(scales)
library(ggplot2)
library(caret)
library(xgboost)

train_read<-read.csv("./train.csv")
head(train_read)
names(train_read)

test_read<-read.csv("./test.csv")
head(test_read)
names(test_read)

#variable respuesta : CHECKUP_RULE_OUT_HIV  

train <- train_read %>% select (-1)
str(train)
summary(train)
glimpse(train)

test <- test_read %>% select (-1)
str(test)
summary(test)
glimpse(test)

#--------------
# TARGET: CHECKUP_RULE_OUT_HIV
#--------------

data <- rbind(train,test)

group <- data %>% mutate(total = n()) %>% group_by(CHECKUP_RULE_OUT_HIV) %>% summarise(prop = n()/max(total))
group

ggplot(group, aes(x = factor(CHECKUP_RULE_OUT_HIV), fill = factor(CHECKUP_RULE_OUT_HIV),y = prop)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de tamizaje de VIH")

#train
group_train <- train %>% mutate(total = n()) %>% group_by(CHECKUP_RULE_OUT_HIV) %>% summarise(prop = n()/max(total))
group_train

ggplot(group_train, aes(x = factor(CHECKUP_RULE_OUT_HIV), fill = factor(CHECKUP_RULE_OUT_HIV),y = prop)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de tamizaje de VIH en data train")

#test
group_test <- test %>% mutate(total = n()) %>% group_by(CHECKUP_RULE_OUT_HIV) %>% summarise(prop = n()/max(total))
group_test

ggplot(group_test, aes(x = factor(CHECKUP_RULE_OUT_HIV), fill = factor(CHECKUP_RULE_OUT_HIV),y = prop)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(prop,accuracy = 0.01)), vjust = 1.5) +
  scale_y_continuous(labels = percent) +
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold",vjust = 2)
  )+
  ggtitle("Proporción de tamizaje de VIH en data test")

#https://juliasilge.com/blog/xgboost-tune-volleyball/


# XGBoost
df<- list()

df$train <- train %>% select(-year)
df$test <- test %>% select(-year)

df$train_mat <- df$train %>% select(-CHECKUP_RULE_OUT_HIV) %>% as.matrix() %>% xgb.DMatrix(data = ., label = df$train$CHECKUP_RULE_OUT_HIV)
df$train_mat

df$test_mat <- df$test %>% select(-CHECKUP_RULE_OUT_HIV) %>% as.matrix() %>% xgb.DMatrix(data = ., label = df$test$CHECKUP_RULE_OUT_HIV)
df$test_mat

#Entrenamiento del modelo
df$modelo_01 <- xgboost(data = df$train_mat,
                        max.depth = 3, # the maximum depth of each decision tree
                        nround = 10, # number of boosting rounds
                        early_stopping_rounds = 3, # if we dont see an improvement in this many rounds, stop
                        objective = "binary:logistic", # the objective function
                        #scale_pos_weight = negative_cases/postive_cases, # control for imbalanced classes
                        gamma = 1)
                       
df$modelo_01

df$predict_01 <- predict(df$modelo_01, df$test_mat)
head(df$predict_01)
summary(df$predict_01)

#Evaluacion del modelo
cbind(df$predict_01> 0.5, df$test$CHECKUP_RULE_OUT_HIV) %>% 
  data.frame() %>% 
  table() %>% 
  confusionMatrix(positive = '1')

# Importance Matrix
importance_matrix <- xgb.importance(model = df$modelo_01)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

# Biblio
#https://medium.com/@jboscomendoza/xgboost-en-r-398e7c84998e
#https://www.kaggle.com/code/rtatman/machine-learning-with-xgboost-in-r/notebook

##################################################################################

library(tidymodels)

# Specify the model and the parameters to tune (parnsip)
xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

#workflow
xgb_wf <- workflow() %>%
  add_formula(CHECKUP_RULE_OUT_HIV ~ .) %>%
  add_model(xgb_spec)
xgb_wf

# Specify the resampling method (rsample)
vb_folds <- vfold_cv(train, v = 2)
vb_folds

# Specify the metrics to optimize (yardstick)
metrics <- metric_set(roc_auc)

# Specify the parameters grid (or you can use dials to automate your grid search)
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  learn_rate(),
  size = 30
)
xgb_grid

# Run each model (tune)
library(doParallel)
registerDoParallel()

set.seed(234)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = vb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

xgb_res

#Explorar resultados
collect_metrics(xgb_res)

# metricas en graficos
xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

show_best(xgb_res, "roc_auc")

best_auc <- select_best(xgb_res, "roc_auc")
best_auc

final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc
)

final_xgb


#codigo:
#https://juliasilge.com/blog/xgboost-tune-volleyball/

##############
# SHAP VALUES
###############
#https://www.r-bloggers.com/2019/03/a-gentle-introduction-to-shap-values-in-r/#:~:text=SHAP%20measures%20the%20impact%20of,with%20and%20without%20the%20feature.
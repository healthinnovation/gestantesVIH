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

plot_data <- group_by(data, year) %>%
  mutate(periodo_size = n()) %>%
  group_by(year, CHECKUP_RULE_OUT_HIV) %>%
  summarise(prop = n()/max(periodo_size))
tail(plot_data)

ggplot(plot_data, aes(x = factor(CHECKUP_RULE_OUT_HIV), fill = factor(CHECKUP_RULE_OUT_HIV), y = prop)) +
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
  ggtitle("Proporción de tamizaje de VIH por periodo")


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

##################
set.seed(1000)
modelo <- xgb.train(data=df$train_mat, params=list(max_depth = 3), nrounds=10)
modelo

Prediccion <- predict(modelo, Datos_P)
Prediccion <- ifelse(Prediccion > 0.5, 1, 0)
M_Confusion <- table(test$CHECKUP_RULE_OUT_HIV, Prediccion)
M_Confusion
confusionMatrix(M_Confusion)

------------------
df$predict_01 <- predict(df$modelo_01, df$test_mat)
head(df$predict_01)

library(caret)
cbind(ifelse(Prediccion > 0.5, 1, 0), test$CHECKUP_RULE_OUT_HIV) %>% 
  data.frame() %>% 
  table() %>% 
  confusionMatrix()

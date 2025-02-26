library(tidyverse)
library(caret)
library(randomForest)
library(ranger)

mean_plot <- data %>%
  abbreviate_long_colnames() %>%
  group_by(Sold) %>%
  summarise(across(all_of(numeric_cols), mean, na.rm = TRUE)) %>%
  select(-ND) %>%
  pivot_longer(-Sold, names_to = "variable", values_to = "mean") %>%
  ggplot(aes(x = factor(Sold), y = mean, fill = factor(Sold))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Mean of Numeric Variables Grouped by Sold",
       x = "Sold (N = Not Sold, Y = Sold)", 
       y = "Mean Value") +
  theme_minimal()

#transform to numeric for binary classification
sold <- data %>%
  mutate(Sold = as.factor(case_when(Sold == "Y" ~ 1,
                          TRUE ~ 0))) %>%
  abbreviate_long_colnames()

#scaling train and test set separately to avoid leakage
set.seed(123)  
trainIndex <- createDataPartition(sold$Sold, p = 0.8, list = FALSE)
train <- sold[trainIndex, ] %>% mutate(across(where(is.numeric), scale))
test  <- sold[-trainIndex, ] %>% mutate(across(where(is.numeric), scale))

#logistic regression model on variables of interest
model <- glm(Sold ~ TotalPremium + WGB + EF + LSB + URB, 
             family = "binomial", data = train,
             weights = ifelse(train$Sold == 1, 2, 1))
summary(model) 

#predicting Sold for test data
test$predicted_prob <- predict(model, newdata = test, type = "response")
test$predicted_class <- as.factor(ifelse(test$predicted_prob > 0.5, 1, 0))
cm_lr <- confusionMatrix(factor(test$predicted_class), factor(test$Sold))

precision_lr <- cm_lr$byClass['Pos Pred Value'] 
recall_lr <- cm_lr$byClass['Sensitivity']        
f1_score_lr <- 2 * (precision_lr * recall_lr) / (precision_lr + recall_lr)

### random forest ###
train_rf <- train %>%
  filter(!is.na(WGB),
         !is.na(EF),
         !is.na(LSB),
         !is.na(URB))

test_rf <- test

#random forest model on variables of interest
rf_model <- randomForest(Sold ~ TotalPremium + EF + LSB + URB,
                         data = train_rf, 
                         ntree = 2000, 
                         mtry = 2,    
                         importance = TRUE)

#predicting Sold for test data
test_rf$rf_pred <- predict(rf_model, newdata = test_rf)
cm_rf <- confusionMatrix(factor(test_rf$rf_pred), factor(test_rf$Sold))

precision_rf <- cm_rf$byClass['Pos Pred Value'] 
recall_rf <- cm_rf$byClass['Sensitivity']        
f1_score_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)

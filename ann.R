knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidymodels)
library(readxl)
library(neuralnet)
library(knitr)

rateDetails <- read_excel("exchangeUSDForecast.xlsx") %>%
  janitor::clean_names() %>%
  mutate(date_in_ymd = ymd(yyyy_mm_dd)) %>%
  select(-1) %>%
  select(date_in_ymd,everything())


#all the input is in only one dataframe to be able to preserve the testing and training
#dataset for the two sets of input variables
laggedRateDetails = rateDetails %>%
  mutate(p_d1_a = lag(rateDetails$usd_eur,1),
         p_d1_b = lag(rateDetails$usd_eur,1),
         p_d1_c = lag(rateDetails$usd_eur,1),
         p_d1_d = lag(rateDetails$usd_eur,1),
         p_d2_a = lag(rateDetails$usd_eur,2),
         p_d2_b = lag(rateDetails$usd_eur,2),
         p_d2_c = lag(rateDetails$usd_eur,2),
         p_d3_a = lag(rateDetails$usd_eur,3),
         five_day_rolling = rollmean(usd_eur,5, fill = NA),
         ten_day_rolling = rollmean(usd_eur,10, fill = NA)) %>%
  
  
  drop_na()

laggedRateDetails %>%
  pivot_longer(cols = 3,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "First Set of Input Variables") +
  theme(legend.position = "none")



laggedRateDetails %>%
  pivot_longer(cols = c(3,4),names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Second Set of Input Variables") +
  theme(legend.position = "none")


laggedRateDetails %>%
  pivot_longer(cols = 7:8,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Third Set of Input Variables") +
  theme(legend.position = "none")

laggedRateDetails %>%
  pivot_longer(cols = 9:10,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Fourth Set of Input Variables") +
  theme(legend.position = "none")

laggedRateDetails %>%
  pivot_longer(cols = 11:12,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Fifth Set of Input Variables") +
  theme(legend.position = "none")

# We can create a function to normalize the data from 0 to 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
# All the variables are normalized
normalized_rate = laggedRateDetails %>%
  mutate(across(2:12, ~normalize(.x)))
# Look at the data that has been normalized
summary(normalized_rate)

set.seed(123)
gbp_train <- normalized_rate[1:400,]
gbp_test <- normalized_rate[401:491,]

# We can create a function to unnormalize the data=
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min ) }
# Get the min and max of the original training values
gbp_min_train <- min(laggedRateDetails[1:400,2])
gbp_max_train <- max(laggedRateDetails[1:400,2])
# Get the min and max of the original testing values
gbp_min_test <- min(laggedRateDetails[401:491,2])
gbp_max_test <- max(laggedRateDetails[401:491,2])
# Check the range of the min and max of the training dataset
gbp_min_test

gbp_min_train

gbp_max_test
gbp_max_train
relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}



relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}





relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}



set.seed(12345)
# function setup that creates 2 layer model
model_two_hidden_layers = function(hidden) {
  nn_model_true = neuralnet(usd_eur ~ p_d2_c+p_d3_a, data=gbp_train, hidden=c(
    hidden), linear.output=TRUE)
  plot(nn_model_true)
  train_results = compute(nn_model_true,gbp_test[,2:3])
  
  pred <- predict(nn_model_true,gbp_test)
  #view(pred)
  
  validation_df <- data.frame(c(gbp_test$date_in_ymd),c(pred),c(gbp_test$usd_eur))
  #view(validation_df)
  
  ###############################
  p = ggplot() + 
    geom_line(data = validation_df, aes(x = c.gbp_test.date_in_ymd., y = c.pred.), color = "blue") +
    geom_line(data = validation_df, aes(x = c.gbp_test.date_in_ymd., y = c.gbp_test.usd_eur.), color = "red") +
    xlab('Dates') +
    ylab('percent.change')
  #print(p)
  ############################
  
  truthcol = laggedRateDetails[401:491,2]$usd_eur
  predcol = unnormalize(train_results$net.result,gbp_min_train, gbp_max_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "One Hidden Layers") %>%
    mutate(hiddel_layers = paste0(hidden),
           input_set = "D") %>%
    filter(.metric != "rsq")
}
# creation of different models with varying number of nodes
results_two_hidden_layers = bind_rows(
  lapply(1:10, function(n) {
    
        model_two_hidden_layers(n)
    
    
  })) %>%
  
  
  janitor::clean_names()

model_two_hidden_layers(1)

# save the stat indices to a dataframe
set_a_models_two_layers = results_two_hidden_layers %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(set_a_models_two_layers[1:10,])



# Combine the dataframes
#set_a_models = rbind(set_a_models,set_a_models_two_layers)










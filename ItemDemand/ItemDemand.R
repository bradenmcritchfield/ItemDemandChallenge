library(tidyverse)
library(tidymodels)
#library(timek)
library(vroom)

itemtrain <- vroom("train.csv")
itemtest <- vroom("test.csv")

nStores <- max(itemtrain$store)
nItems <- max(itemtrain$item)


storeItemTrain1_1 <- itemtrain %>%
  filter(store==1, item == 1)
storeItemTrain3_17 <- itemtrain %>%
  filter(store==3, item==17)
storeItemTrain6_8 <- itemtrain %>%
  filter(store == 6, item==8)
storeItemTrain10_47 <- itemtrain %>%
  filter(store==10, item == 47)


x1 <- storeItemTrain1_1 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 365) +
  ggtitle("Autocorrelation for Store 1, Item 1")

x2 <- storeItemTrain3_17 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 365) +
  ggtitle("Autocorrelation for Store 3, Item 17")


x3 <- storeItemTrain6_8 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 365) +
  ggtitle("Autocorrelation for Store 6, Item 8")


x4 <- storeItemTrain10_47 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 365) +
  ggtitle("Autocorrelation for Store 10, Item 47")

gridExtra::grid.arrange(x1,x2,x3,x4)
  
###########################################
storeItem <- itemtrain %>%
  filter(store == 4, item == 14)
storeItem


myrecipe <- recipe(sales ~ ., data = storeItem) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  #step_date(date, features ="decimal") %>%
  step_date(date, features = "year") %>%
  step_rm(store, item) %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min = 0, max=pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_holiday(date, holidays = "ChristmasDay")

prepped_recipe <- prep(myrecipe)      
baked_recipe <- bake(prepped_recipe, new_data = itemtrain)


#################################################################################
# Random Forests
#################################################################################
#install.packages("ranger")
library(tidymodels)
my_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees=500) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression") 
## Create a workflow with model & recipe
randfor_wf <- workflow() %>%
  add_recipe(myrecipe) %>%
  add_model(my_mod)
## Set up grid of tuning values
tuning_grid <- grid_regular(mtry(range = c(1,(ncol(storeItem)-1))), min_n(), levels = 5)
## Set up K-fold CV
folds <- vfold_cv(storeItem, v = 5, repeats = 1)
## Find best tuning parameters
CV_results <- randfor_wf %>%
  tune_grid(resamples = folds, grid = tuning_grid, metrics=metric_set(smape))


bestTune <- CV_results %>%
  select_best("smape")

collect_metrics(CV_results) %>%
  filter(mtry==bestTune$mtry, min_n == bestTune$min_n) %>%
  pull(mean)
  

################################################
# Exponential Smoothing
#### ### ### ### ### ### ### ### ### ### ### ###

library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions

########################################
#Store 4, Item 4

train <- itemtrain %>% filter(store==4, item==14)
test <- itemtrain %>% filter(store==4, item==14)
cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)
cv_split %>%
tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

es_model <- exp_smoothing() %>%
set_engine("ets") %>%
fit(sales~date, data=training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

## Visualize CV results9
p1 <- cv_results %>%
modeltime_forecast(
                   new_data = testing(cv_split),
                   actual_data = train
) %>%
plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
modeltime_accuracy() %>%
table_modeltime_accuracy(
                         .interactive = FALSE
)

## Refit to all data then forecast1
es_fullfit <- cv_results %>%
modeltime_refit(data = train)
es_preds <- es_fullfit %>%
modeltime_forecast(h = "3 months") %>%
rename(date=.index, sales=.value) %>%
select(date, sales) %>%
full_join(., y=itemtest, by="date") %>%
select(id, sales)

p2 <- es_fullfit %>%
modeltime_forecast(h = "3 months", actual_data = train) %>%
plot_modeltime_forecast(.interactive=FALSE)

##################################################
# Store 8, Item 42
train <- itemtrain %>% filter(store==8, item==42)
cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)
cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

## Visualize CV results
p3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Refit to all data then forecast1
es_fullfit <- cv_results %>%
  modeltime_refit(data = train)
es_preds <- es_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=itemtest, by="date") %>%
  select(id, sales)

p4 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = train) %>%
  plot_modeltime_forecast(.interactive=FALSE)


plotly::subplot(p1,p3,p2,p4, nrows=2)

##############################################################
#SARIMA

train <- itemtrain %>% filter(store==1, item==1)
test <- itemtest %>% filter(store==1, item==1)
cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)
#cv_split %>%
 # tk_time_series_cv_plan() %>% #Put into a data frame
#plot_time_series_cv_plan(date, sales, .interactive=FALSE)

arima_recipe <- recipe(sales ~ ., data = storeItem) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features ="decimal") %>%
  step_date(date, features = "year") %>%
  #step_rm(store, item) %>%
  step_date(date, features="doy") %>%
  #step_range(date_doy, min = 0, max=pi) %>%
  #step_mutate(sinDOY = sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_holiday(date, holidays = "ChristmasDay") # For the linear model part

arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=10, # default max p to tune
                         non_seasonal_ma=10, # default max q to tune
                         seasonal_ar=10, # default max P to tune
                         seasonal_ma=10, #default max Q to tune
                         non_seasonal_differences=10, # default max d to tune
                         seasonal_differences=10 #default max D to tune
) %>%
set_engine("auto_arima")


arima_wf <- workflow() %>%
add_recipe(arima_recipe) %>%
add_model(arima_model) %>%
fit(data=training(cv_split))

## Calibrate (i.e. tune) workflow
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))

## Visualize & Evaluate CV accuracy
p1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Refit best model to entire data and predict


ar_fullfit <- cv_results %>%
  modeltime_refit(data = train)
p2 <- ar_fullfit %>%
  modeltime_forecast(new_data=test, actual_data = train) %>%
  plot_modeltime_forecast(.interactive=FALSE)
 

train <- itemtrain %>% filter(store==7, item==16)
test <- itemtest %>% filter(store==7, item==16)
cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)
#cv_split %>%
# tk_time_series_cv_plan() %>% #Put into a data frame
#plot_time_series_cv_plan(date, sales, .interactive=FALSE)

arima_recipe <- recipe(sales ~ ., data = storeItem) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features ="decimal") %>%
  step_date(date, features = "year") %>%
  #step_rm(store, item) %>%
  step_date(date, features="doy") %>%
  #step_range(date_doy, min = 0, max=pi) %>%
  #step_mutate(sinDOY = sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_holiday(date, holidays = "ChristmasDay") # For the linear model part

arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=10, # default max p to tune
                         non_seasonal_ma=10, # default max q to tune
                         seasonal_ar=10, # default max P to tune
                         seasonal_ma=10, #default max Q to tune
                         non_seasonal_differences=10, # default max d to tune
                         seasonal_differences=10 #default max D to tune
) %>%
  set_engine("auto_arima")


arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))

## Calibrate (i.e. tune) workflow
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))

## Visualize & Evaluate CV accuracy
p3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Refit best model to entire data and predict


ar_fullfit <- cv_results %>%
  modeltime_refit(data = train)
p4 <- ar_fullfit %>%
  modeltime_forecast(new_data=test, actual_data = train) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(p1,p3,p2,p4, nrows=2)

###############################################################
# Facebook Prophet Model
train <- itemtrain %>% filter(store==1, item==1)
test <- itemtest %>% filter(store==1, item==1)
cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)

arima_recipe <- recipe(sales ~ ., data = storeItem) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features ="decimal") %>%
  step_date(date, features = "year") %>%
  #step_rm(store, item) %>%
  step_date(date, features="doy") %>%
  #step_range(date_doy, min = 0, max=pi) %>%
  #step_mutate(sinDOY = sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_holiday(date, holidays = "ChristmasDay") 

prophet_model <- prophet_reg() %>%
set_engine(engine = "prophet") %>%
fit(sales ~ date, data = training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))

## Visualize CV results
p1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Refit to all data then forecast1
fp_fullfit <- cv_results %>%
  modeltime_refit(data = train)
fp_preds <- fp_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=itemtest, by="date") %>%
  select(id, sales)

p2 <- fp_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = train) %>%
  plot_modeltime_forecast(.interactive=FALSE)



train <- itemtrain %>% filter(store==7, item==14)
test <- itemtest %>% filter(store==7, item==14)
cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)

prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))

## Visualize CV results
p3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Refit to all data then forecast1
fp_fullfit <- cv_results %>%
  modeltime_refit(data = train)
fp_preds <- fp_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=itemtest, by="date") %>%
  select(id, sales)

p4 <- fp_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = train) %>%
  plot_modeltime_forecast(.interactive=FALSE)



plotly::subplot(p1,p3,p2,p4, nrows=2)



############################################
#Final Model
#### ### ### ### ### ### ### ### ### ### ###
## Libraries
library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(vroom)
library(embed)
library(bonsai)
library(lightgbm)

## Read in the data
item <- vroom::vroom("train.csv")
itemTest <- vroom::vroom("test.csv")
n.stores <- max(item$store)
n.items <- max(item$item)

## Define the workflow
item_recipe <- recipe(sales~., data=item) %>%
  step_date(date, features=c("dow", "month", "decimal", "doy", "year")) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome=vars(sales)) %>%
  step_holiday(date, holidays = c("ChristmasDay","USThanksgivingDay")) %>%
  step_rm(date, item, store) %>%
  step_normalize(all_numeric_predictors())

boosted_model <- boost_tree(tree_depth=2, #Determined by random store-item combos
                            trees=2000,
                            learn_rate=0.01) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")
boost_wf <- workflow() %>%
  add_recipe(item_recipe) %>%
  add_model(boosted_model)

## Double Loop over all store-item combos
for(s in 1:n.stores){
  for(i in 1:n.items){
    
    ## Subset the data
    train <- item %>%
      filter(store==s, item==i)
    test <- itemTest %>%
      filter(store==s, item==i)
    
    ## Fit the data and forecast
    fitted_wf <- boost_wf %>%
      fit(data=train)
    preds <- predict(fitted_wf, new_data=test) %>%
      bind_cols(test) %>%
      rename(sales=.pred) %>%
      select(id, sales)
    
    ## Save the results
    if(s==1 && i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds,
                             preds)
    }
    
  }
}


vroom_write(x=all_preds, "./submission.csv", delim=",")

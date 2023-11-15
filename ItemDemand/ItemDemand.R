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
  


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
  
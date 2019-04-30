library(dplyr)
library(ggplot2)
library(forcats)
library(mlr)
library(tidyr)

################## Original data

f <- read.csv("train.csv")

f %>% filter(!is.na(FinelineNumber), !is.na(Upc)) -> f

################## FUNCTIONS

sumNegative <- function(x) {
  sum(x[x<0]) 
}

entropia<-function(x){
  x<-x[x>0]
  s<-sum(x)
  y<-x/s
  w<-y*log(y)
  e<-sum(w)
  -e
}
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

################### My SUPERB dataset

freq_superb <- f %>% 
  group_by(TripType,
           Weekday,
           VisitNumber,
           DepartmentDescription) %>% 
  summarise(liczba_unikalnych_produktow = length(unique(Upc)), ###
            ScanCountSum = sum(ScanCount),
            ScanSumNeg = sumNegative(ScanCount)) %>%
  group_by(TripType, Weekday, VisitNumber) %>% 
  summarise(liczba_kupionych_produktow = sum(ScanCountSum),
            liczba_unikalnych_produktow = sum(liczba_unikalnych_produktow), ###
            liczba_departamentow = length(unique(DepartmentDescription)),
            liczba_zwroconych = sumNegative(ScanSumNeg),
            entropia = entropia(ScanCountSum), ###
            glowny_dzial = getmode(DepartmentDescription))

freq_superb<-freq_superb %>% select(-VisitNumber)

################## Ranger

ranger <- makeLearner("classif.ranger", predict.type = "prob", par.vals = list(num.trees=40))

################## SEED

set.seed(120)

################## OLD

old_data_task <- makeClassifTask(id = "firstTask", data = f, target = "TripType")

train_index_old <- sample(1:nrow(f), 0.8 * nrow(f))
test_index_old <- setdiff(1:nrow(f), train_index_old)

ranger_model_old <- train(ranger, old_data_task, subset = train_index_old)
ranger_prediction_old <- predict(ranger_model_old, old_data_task, subset = test_index_old)

regularPerformance <- performance(ranger_prediction_old,
                                 measures = list(
                                   logloss,
                                   mmce,
                                   acc))

################# NEW

new_data_task <- makeClassifTask(id = "newTask", data = freq_superb, target = "TripType")

train_index_new <- sample(1:nrow(freq_superb), 0.8 * nrow(freq_superb))
test_index_new <- setdiff(1:nrow(freq_superb), train_index_new)

ranger_model_new <- train(ranger, new_data_task, subset = train_index_new)
ranger_prediction_new <- predict(ranger_model_new, new_data_task, subset = test_index_new)

superbPerformance <- performance(ranger_prediction_new,
            measures = list(
              logloss,
              mmce,
              acc
            ))

################ SAVING

save(superbPerformance, file = "sup.rda")
save(regularPerformance, file = "reg.rda")


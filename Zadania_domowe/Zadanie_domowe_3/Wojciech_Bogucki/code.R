library(data.table)
library(dplyr)
library(DataExplorer)
library(funModeling)
library(ranger)
library(mlr)
library(mlrCPO)


# wczytanie danych i stworzenie nowych kolumn -----------------------------


data_train <- fread("train.csv", data.table = FALSE)

data_train <- data_train %>% na.omit %>% mutate_if(is.character, as.factor) 

data_train_new <- data_train %>% 
  group_by(VisitNumber) %>% 
  mutate(weekend=ifelse(Weekday %in% c("Saturday","Sunday"),1L,0L),    #czy zakup byl dokonany w weekend
         returned=ifelse(ScanCount<0,1L,0L),                           #czy towar zostal zwrocony
         count=n())                                                    #ile roznych produktow kupiono podczas tych zakupow


# podzial na zrobior teningowy i testowy ----------------------------------


n <- nrow(data_train)
samp_tr <- sample(1:n,floor(0.6*n))
data_tr <- data_train[samp_tr,]
data_ts <- setdiff(data_train, data_tr)
data_tr_new <- data_train_new[samp_tr,]
data_ts_new <- setdiff(data_train_new, data_tr_new)



# predykcja ---------------------------------------------------------------

set.seed(1234,"L'Ecuyer")

task1 <- makeClassifTask(id = "task1",data = data_tr, target="TripType")
task2 <- makeClassifTask(id = "task2",data = data_tr_new, target="TripType")
learner <- makeLearner("classif.ranger", predict.type = "prob", par.vals = list(num.trees=50))

train <- train(learner = learner,task = task1)
train2 <- train(learner = learner,task = task2)

pred <- predict(train,newdata=data_ts)
pred2 <- predict(train2,newdata=data_ts_new)
conf_matrix <- table(pred$data$response, data_ts$TripType)
conf_matrix2 <- table(pred2$data$response, data_ts_new$TripType)

#accuracy
sum(diag(conf_matrix))/sum(conf_matrix)
#0.3741925
sum(diag(conf_matrix2))/sum(conf_matrix2)
#0.5910072


#logloss
probs1 <- as.matrix(select(pred$data,-truth, -response))
colnames(probs1) <- levels(pred1$data$truth)
measureLogloss(probs1,pred$data$truth)
#2.278243
probs2 <- as.matrix(select(pred2$data,-truth, -response))
colnames(probs2) <- levels(pred2$data$truth)
measureLogloss(probs2,pred2$data$truth)
#1.560171
library(mlr)
library(dplyr)
library(ggplot2)
library(funModeling)
library(DataExplorer)
library(patchwork)
library(DataExplorer)

set.seed(1)

dat <- read.csv("train.csv", sep = ",")
test_dat <- read.csv("test.csv", sep=",")

head(dat)
newDat <- dat %>% 
  group_by(VisitNumber) %>%
  summarise(TripType = head(TripType,1),
            count = n(),
            day = unique(Weekday)[1])

newTestDat <- test_dat %>% select(Weekday)
classifTask <- makeClassifTask(id = "tsk", data = newDat, target = "TripType")
classifLrn <- makeLearner("classif.ranger", predict.type = "prob")
cv <- makeResampleDesc("CV", iters = 5)
r <- resample(classifLrn, classifTask, measures = acc, resampling = cv)

head(newDat)
newCol <- dat %>% group_by(VisitNumber) %>% count(DepartmentDescription) %>% slice(which.max(n))
newDat2 <- cbind(newDat, newCol$DepartmentDescription)
colnames(newDat2)[5] <- "MostCommonDepartment"

classifTask1 <- makeClassifTask(id = "tsk2", data = newDat2, target = "TripType")
classifLrn1 <- makeLearner("classif.ranger", predict.type = "prob")
r1 <- resample(classifLrn1, classifTask1, measures = acc, resampling = cv)

newCol1 <- dat %>% group_by(VisitNumber) %>% mutate(ReturnedItem=if_else(sum(ScanCount)!=sum(abs(ScanCount)), 1, 0))
newCol1 <- newCol1 %>% group_by(VisitNumber) %>% summarise(ReturnedItem = mean(ReturnedItem))
newDat3 <- cbind(newDat2, newCol1$ReturnedItem)
colnames(newDat3)[6] <- "ReturnedItem"

classifTask2 <- makeClassifTask(id = "tsk3", data = newDat3, target = "TripType")
classifLrn2 <- makeLearner("classif.ranger", predict.type = "prob")
r2 <- resample(classifLrn2, classifTask2, measures = acc, resampling = cv)

newCol2 <- dat %>% 
  group_by(VisitNumber) %>%
  summarise(TotalCount = sum(ScanCount))
newDat4 <- cbind(newDat3, newCol2$TotalCount)
colnames(newDat4)[7] <- "TotalCount"

classifTask3 <- makeClassifTask(id = "tsk4", data = newDat4, target = "TripType")
classifLrn3 <- makeLearner("classif.ranger", predict.type = "prob")
r3 <- resample(classifLrn3, classifTask3, measures = acc, resampling = cv)

test_set <- sample_frac(newDat, 0.2)
train_set <- setdiff(newDat, test_set)

classifTaskL <- makeClassifTask(id = "tskL", data = train_set, target = "TripType")

trained1 <- train(classifLrn, classifTaskL)
predicted1 <- predict(trained1, newdata = test_set)

test_set1 <- sample_frac(newDat4, 0.2)
train_set1 <- setdiff(newDat4, test_set1)

classifTaskL2 <- makeClassifTask(id = "tskL2", data = train_set1, target = "TripType")
trained2 <- train(classifLrn, classifTaskL2)
predicted2 <- predict(trained2, newdata = test_set1)

cols1 <- paste0("prob.", predicted1$data$truth)
rows1 <- 1:nrow(predicted1$data)
temp1 <- as.numeric(predicted1$data[cbind(rows1, cols1)])
temp1 <- as.numeric((lapply(temp1, function(x) max(min(x, 1-10e-15), 10e-15))))
err1 <- -sum(log(temp1))/nrow(predicted1$data)

cols2 <- paste0("prob.", predicted2$data$truth)
rows2 <- 1:nrow(predicted2$data)
temp2 <- as.numeric(predicted2$data[cbind(rows2, cols2)])
temp2 <- as.numeric((lapply(temp2, function(x) max(min(x, 1-10e-15), 10e-15))))
err2 <- -sum(log(temp2))/nrow(predicted2$data)


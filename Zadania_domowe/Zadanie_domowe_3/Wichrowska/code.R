library(dplyr)
library(mlr)

data <- read.csv("train.csv", sep = ",")


# Funckje pomocnicze

show_acc <- function (data) {
  
  classifTask <- makeClassifTask(id = "task", data = data, target = "TripType")
  classifLrn <- makeLearner("classif.ranger", predict.type = "prob")
  cv <- makeResampleDesc("CV", iters = 5)
  r <- resample(classifLrn, classifTask, cv, measures = acc)
  r$aggr
  
}

show_error <- function (data) {
  
  test <- sample_frac(data, 0.2)
  train <- setdiff(data, test)
  
  classifTask <- makeClassifTask(id = "task", data = train, target = "TripType")
  classifLrn <- makeLearner("classif.ranger", predict.type = "prob")
  trained <- train(classifLrn, classifTask)
  predicted <- predict(trained, newdata = test)
  
  cols <- paste0("prob.", predicted$data$truth)
  rows <- 1:nrow(predicted$data)
  temp <- as.numeric(predicted$data[cbind(rows, cols)])
  temp <- as.numeric((lapply(temp, function(x) max(min(x, 1-10e-15), 10e-15))))
  error <- -sum(log(temp))/nrow(predicted$data)
  
}


# Podstawowa ramka danych

data2 <- data %>% 
  group_by(VisitNumber) %>%
  summarise(TripType = head(TripType,1),
            count = n(),
            day = unique(Weekday)[1])

acc2 = show_acc(data2)
error2 = show_error(data2)

# Dodanie kolumny dotyczącej najczęściej odwiedzanego działu w sklepie

bestDepartment <- data %>% 
  group_by(VisitNumber) %>% 
  count(DepartmentDescription) %>% 
  slice(which.max(n))

data3 <- cbind(data2, bestDepartment$DepartmentDescription)
colnames(data3)[5] <- "bestDepartment"

acc3 = show_acc(data3)
error3 = show_error(data3)

# Dodanie kolumny dotyczącej sumy wszystkich produktów

productCount <- data %>% 
  group_by(VisitNumber) %>% 
  summarise(count = sum(ScanCount))

data4 <- cbind(data3, productCount$count)
colnames(data4)[6] <- "productCount"

acc4 = show_acc(data4)
error4 = show_error(data4)


# Dodanie kolumny dotyczącej sumy zwróconych produktów

returnedCount <- data %>% 
  group_by(VisitNumber) %>% 
  summarise(count = sum(ScanCount < 0))


data5 <- cbind(data4, returnedCount$count)
colnames(data5)[7] <- "returnedCount"

acc5 = show_acc(data5)
error5 = show_error(data5)


data6 <- cbind(data4, ifReturned)
colnames(data6)[7] <- "ifReturned"

acc6 = show_acc(data6)
error6 = show_error(data6)

library(readr)
library(DataExplorer)
library(dplyr)
library(mlr)
set.seed(1)
moz<-listLearners(check.packages = TRUE)
moz<-moz[moz$type=="classif",]
moz<-moz[moz$prob==TRUE,]
moz<-moz[moz$multiclass==TRUE,]
setwd("~/Desktop/zadanie3WUM/kaggle")

train <- read_csv("train.csv")
#train<-sample_frac(train,0.02)
wizyty<-unique(train$VisitNumber)
wizyty<-sample(wizyty,size = floor(length(wizyty)/55),replace = FALSE)
train<-train[train$VisitNumber %in% wizyty,]
train<-filter(train,!is.na(FinelineNumber)) %>% select(-Upc)
train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)],as.factor) 
train$TripType<-factor(train$TripType)                                             
#train$ScanCount<-sapply(train$ScanCount,function(x){ifelse(x<0,-1,x)})


#DataExplorer::create_report(train)

model<-"classif.gbm"

task = makeClassifTask(id = "task", data = train,"TripType" )
learner<-makeLearner(model)

cv <- makeResampleDesc("CV", iters = 5)
r1 <- resample(learner, task, cv,measures = acc)

train %>% group_by(VisitNumber) %>% summarise(numberofreturns=sum(ScanCount<0),
                                                        diffrentDep=length(unique(DepartmentDescription)),
                                                        diffrentProd=length(unique(FinelineNumber)))->temp


train<-inner_join(train,xd,by="VisitNumber")
task = makeClassifTask(id = "task", data = train,"TripType" )
r2 <- resample(learner, task, cv,measures = acc)



set.seed(123, "L'Ecuyer")
library(mlr)
library(readr)
library(DataExplorer)
library(dplyr)
library(rpart)
library(rpart.plot)

train <- read_csv("titanic/kaggle-titanic-master/input/train.csv")
train<-drop_columns(train,c("Name","PassengerId","Ticket"))
train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)],as.factor) 
train$Survived<-as.factor(as.logical(train$Survived))
train$Cabin<-as.factor(!is.na(train$Cabin))

#robimy taska i learnera
task <- makeClassifTask(id = "task", data = train, target = "Survived")
learner_default<-makeLearner("classif.rpart",predict.type = "prob")
getParamSet(learner_default)
learner_article<-makeLearner("classif.rpart",predict.type = "prob",par.vals = list(cp=0.001,maxdepth=13,minbucket=12,minsplit=18))

tree<-mlr::train(learner_article,task)

rpart_pars <- tuneParams(
  makeLearner("classif.rpart",predict.type = "prob"),
  subsetTask(makeClassifTask(id = "task", data = train, target = "Survived")),
  resampling = cv5,
  measures = mlr::auc,
  par.set = makeParamSet(
    makeNumericParam("cp",lower = 0,upper = 1),
    makeDiscreteParam("maxdepth", values = 1:30),
    makeDiscreteParam("minbucket", values = 1:40),
    makeDiscreteParam("minsplit", values = 1:40)
  ),
  control = makeTuneControlRandom(maxit = 200)
  )

learner_random<-makeLearner("classif.rpart",predict.type = "prob",par.vals = rpart_pars$x)

#nasze hiperparametry ktore zmieniamy to:
#maxdepth czyli maksymalna głębokośc drzewa, gdzie korzen uznajemy ze ma głębokość 0
#minsplit to minimalna ilosc obserwacji dla ktorej mozemy dalej dzielic dany węzeł
#minbucket to minimalna ilość obserwacji w lisciu
#cp to stopień złożoności drzewa. Oznacza to że jeżeli podział węzła nie zmiejsza ogólnego braku dopasowania o co najmniej cp to się go nie dzieli.  

#testy Acc, AUC, Specificity, Recall, Precision, F1 
cv <- makeResampleDesc("CV", iters = 5)
r_article <- resample(learner_article, task, cv,measures = list(acc,auc,tnr,tpr,ppv,f1),extract = function(x){getLearnerModel(x)},show.info = FALSE)
rpart.plot(r_article$extract[[which.max(r_article$measures.test$auc)]])
r_article$aggr

r_default <- resample(learner_default, task, cv,measures = list(acc,auc,tnr,tpr,ppv,f1))
r_default<-r_default$aggr
r_default

rpart.plot(tree$learner.model)
plot(tree)
print(tree)

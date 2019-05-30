library(dplyr)
library(jsonlite)
library(mlr)
library(stringi)
library(ggplot2)
options(stringsAsFactors = FALSE)

directories <-list.dirs(recursive = TRUE)
directories<-directories[stri_detect_regex(directories,pattern  = "[0-9a-z]{32}$")]
i<-0
classification_model <-list()
classification_audit <-list()
classification_dataset <- list()
classification_task <- list()

for (dir in directories) {
  audit <- read_json(path = file.path(dir,"audit.json"),simplifyVector = TRUE)
  model <- read_json(path = file.path(dir,"model.json"),simplifyVector = TRUE)
  dataset <- read_json(path = file.path(dirname(dirname(dir)),"dataset.json"),simplifyVector = TRUE)
  task <- read_json(path = file.path(dirname(dir),"task.json"),simplifyVector = TRUE)
  if(stri_detect_regex(model$task_id,pattern = "^classification")){
    i<-i+1
    classification_model[i]<-list(model)
    classification_audit[i]<-list(audit)
    classification_dataset[i]<-list(dataset)
    classification_task[i] <- list(task)
  }
}

classification <- tibble(id=character(),added_by=character(),date=character(),library=character(),model_name=character(),task_id=character(),
                         dataset_id=character(), number_of_features=numeric(), number_of_instances=numeric(), frequency1=numeric(), frequency2=numeric(),
                         acc=numeric(),specificity=numeric(),recall=numeric(),precision=numeric(),f1=numeric())


for(x in 1:i){
  colnames(classification_dataset[[x]]$variables[[classification_task[[x]]$target]]$cat_frequencies) <- c('frequency1', 'frequency2')
  classification<-rbind(classification,c(classification_model[[x]][1:7],classification_dataset[[x]][7:8], classification_dataset[[x]]$variables[[classification_task[[x]]$target]]$cat_frequencies,
classification_audit[[x]]$performance))}

#filtrowanie
classification<-classification[!is.na(classification$acc),]

unique(classification$model_name)
table(classification$model_name)

classification %>% group_by(model_name) %>% filter(n()>3)-> classification_filter

classification_filter %>% group_by(task_id,dataset_id) %>% filter(n()>8)-> classification_filter

classification_filter %>% group_by(model_name) %>% filter(n()>3)-> classification_filter

classification_filter$task_id<-substring(classification_filter$task_id,first = 16)

classification_filter$model_name<-substring(classification_filter$model_name,first = 9)

unique(classification_filter$model_name)
table(classification_filter$model_name)

ggplot(classification_filter,aes(y=acc,x=model_name,group=dataset_id,fill=model_name))+geom_bar(stat = "identity",position = "dodge")+facet_grid(.~dataset_id,scales="free")+theme(axis.text.x = element_text(angle = 45))

model<-"regr.svm"

train <-classification_filter[-c(1,2,3,4,6)]

train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)], as.factor)

set.seed(1)
train<-sample_frac(train)
task = makeRegrTask(id = "task", data = train, "acc")
learner<-makeLearner(model)
measures<-intersect(listMeasures(task),c("mse","rsq","mae","rmse"))
Rcuda<-list(mse=mse,rmse=rmse, mae=mae,rsq=rsq)
cv <- makeResampleDesc("CV", iters = 5)
r <- resample(learner, task, cv,measures = Rcuda[measures])


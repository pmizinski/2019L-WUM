library(OpenML)
library(DALEX)
set.seed(123, "L'Ecuyer")
#dane<-getOMLDataSet(40498)
#write.csv(train,"wine.csv") 
wina<-read.csv("wine.csv")
wina$Class<-factor(wina$Class)
wina<-wina[-1]
domy<-DALEX::apartments

#getParamSet(wina_learner)

wina_task = makeRegrTask(id = "wina", data = wina, target ="V1")
wina_learner<-makeLearner("regr.svm")

domy_task = makeRegrTask(id = "domy", data = domy, target ="m2.price")
domy_learner<-makeLearner("regr.svm")

cv <- makeResampleDesc("CV", iters = 5)
test_wina <- resample(wina_learner, wina_task, cv,measures = rmse)
test_domy <- resample(domy_learner, domy_task, cv,measures = rmse)

domy_learner<-makeLearner("regr.svm",par.vals = list(scale=FALSE))

test_domy <- resample(domy_learner, domy_task, cv,measures = rmse,show.info = FALSE)

print(test_domy)
# TEST

train_index <- sample(1:nrow(domy), 0.8 * nrow(domy))

dalex_train_domy <- domy[train_index,]
dalex_test_domy <- domy[-train_index,]

dalex_svm_domy <- train(domy_learner, domy_task, subset=train_index)

custom_predict<-function(object, newdata) {pred <- predict(object, newdata=newdata)
response <- pred$data$response
return(response)}


explainer_regr_svm <- DALEX::explain(dalex_svm_domy,data=dalex_test_domy[-1] ,custom_predict,
                                        y=dalex_test_domy[[1]], label ="svm")

plot(DALEX::single_variable(explainer_regr_svm,variable = "construction.year",type = "ale"))

variable_importance(explainer_regr_svm, loss_function = loss_root_mean_square)
plot(model_performance(explainer_regr_svm))

svm_pars <- tuneParams(
  makeLearner("classif.svm", predict.type = "prob"),
  subsetTask(domy_task),
  resampling = cv5,
  measures = mlr::acc,
  par.set = makeParamSet(
    makeDiscreteParam("cost", values = seq(0.1,4,by=0.2)),
    makeDiscreteParam("kernel", values = c("linear","polynomial","radial","sigmoid"))
  ),
  control = makeTuneControlRandom(maxit = 20)
)




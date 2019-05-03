library(mlr)
library(readr)
library(dplyr)

# Wczytanie danych

titanic <- read_csv("titanic.csv")

titanic <- titanic %>% select(-name,
                              -boat,
                              -body,
                              -home.dest,
                              -ticket,
                              -cabin)

# Przygotowanie danych

titanic$sex <- titanic$sex %>% as.factor
titanic$embarked<- titanic$embarked %>% as.factor

titanic %>% filter(age != "?") -> titanic
titanic$age <- titanic$age %>% as.numeric

titanic %>% filter(embarked != "?") -> titanic
titanic$embarked <- titanic$embarked %>% droplevels

titanic$survived <- titanic$survived %>% as.logical

# Podział zbioru

index <- sample(1:nrow(titanic), nrow(titanic)/5)
train <- titanic[-index, ]
test <- titanic[index, ]

# Task

task <- makeClassifTask(id = "task",
                        data = train,
                        target = "survived")

# CV

cv <- makeResampleDesc(method = "CV", iters = 5)

# Measures

measures <- list(auc, acc, f1)

# Tunning parametrów 

ctrl <- makeTuneControlGrid()
rand <- makeTuneControlRandom(maxit = 500)

parms <- makeParamSet(
  makeNumericParam("cp", lower = 0, upper = 0.01),
  makeIntegerParam("maxdepth", lower = 10, upper = 30),
  makeIntegerParam("minbucket", lower = 3.5, upper = 45),
  makeIntegerParam("minsplit", lower = 4, upper = 55)
)

parms_ctree <- makeParamSet(
  makeNumericParam("mincriterion", lower = 0.01, upper = 0.999),
  makeIntegerParam("maxdepth",lower = 0, upper = 20),
  makeIntegerParam("minbucket", lower = 1, upper = 15),
  makeIntegerParam("minsplit", lower = 5, upper = 40)
)
# 
# res <- tuneParams(learner = makeLearner("classif.rpart", predict.type = "prob"),
#                   task = task,
#                   resampling = cv,
#                   measures = measures,
#                   par.set = parms,
#                   control = ctrl)

# save(res, file ="res.rda")
load("res.rda")

# res_rand <- tuneParams(learner = makeLearner("classif.rpart", predict.type = "prob"),
#                   task = task,
#                   resampling = cv,
#                   measures = measures,
#                   par.set = parms,
#                   control = rand)
# save(res_rand, file ="res_rand.rda")
load("res_rand.rda")

# res_ctree <- tuneParams(learner = makeLearner("classif.ctree", predict.type = "prob"),
#                         task = task,
#                         resampling = cv,
#                         measures = measures,
#                         par.set = parms_ctree,
#                         control = ctrl)
# 
# save(res_ctree, file = "res_ctree.rda")
load("res_ctree.rda")

# res_ctree_rand <- tuneParams(learner = makeLearner("classif.ctree", predict.type = "prob"),
#                         task = task,
#                         resampling = cv,
#                         measures = measures,
#                         par.set = parms_ctree,
#                         control = rand)
# 
# save(res_ctree_rand, file = "res_ctree_rand.rda")
load("res_ctree_rand.rda")

# Learners

rpart_default <- makeLearner("classif.rpart", predict.type = "prob")
rpart_new_def <- makeLearner("classif.rpart",
                             predict.type = "prob",
                             par.vals = list(
                               cp = 0,
                               maxdepth = 21,
                               minbucket = 12,
                               minsplit = 24
                             ))

rpart_ctrl <- setHyperPars(rpart_default,
                           par.vals = res$x)
rpart_rand <- setHyperPars(rpart_default,
                           par.vals = res_rand$x)

rpart_new_def_gini <- makeLearner("classif.rpart",
                             predict.type = "prob",
                             par.vals = list(
                               cp = 0,
                               maxdepth = 21,
                               minbucket = 12,
                               minsplit = 24,
                               parms = list(split = "gini")
                             ))
rpart_new_def_infg <- makeLearner("classif.rpart",
                             predict.type = "prob",
                             par.vals = list(
                               cp = 0,
                               maxdepth = 21,
                               minbucket = 12,
                               minsplit = 24,
                               parms = list(split = "information")
                             ))

ctree_default <- makeLearner("classif.ctree",
                             predict.type = "prob") 

ctree_ctrl <- setHyperPars(ctree_default,
                           par.vals = res_ctree$x)

ctree_rand <- setHyperPars(ctree_default,
                           par.vals = res_ctree_rand$x)

# Models 

model_default <- train(rpart_default, task = task)
model_new_def <- train(rpart_new_def, task = task)
model_ctrl <- train(rpart_ctrl, task = task)
model_rand <- train(rpart_rand, task = task)

model_new_def_gini <- train(rpart_new_def_gini, task = task)
model_new_def_infg <- train(rpart_new_def_infg, task = task)

model_ctree_default <- train(ctree_default, task = task)
model_ctree_ctrl <- train(ctree_ctrl, task = task)
model_ctree_rand <- train(ctree_rand, task = task)

# Test

pred_default <- predict(model_default, newdata = test)
pred_new_def <- predict(model_new_def, newdata = test)
pred_ctrl <- predict(model_ctrl, newdata = test)
pred_rand <- predict(model_rand, newdata = test)

pred_new_def_gini <- predict(model_new_def_gini, newdata = test)
pred_new_def_infg <- predict(model_new_def_infg, newdata = test)

pred_ctree_default <- predict(model_ctree_default, newdata = test)
pred_ctree_ctrl <- predict(model_ctree_ctrl, newdata = test)
pred_ctree_rand <- predict(model_ctree_rand, newdata = test)

# Performance

per_default <- performance(pred = pred_default, measures = measures)
per_new_def <- performance(pred = pred_new_def, measures = measures)
per_ctrl <- performance(pred = pred_ctrl, measures = measures)
per_rand <- performance(pred = pred_rand, measures = measures)

per_new_def_gini <- performance(pred = pred_new_def_gini, measures = measures)
per_new_def_infg <- performance(pred = pred_new_def_infg, measures = measures)

per_ctree_default <- performance(pred = pred_ctree_default, measures = measures)
per_ctree_ctrl <- performance(pred = pred_ctree_ctrl, measures = measures)
per_ctree_rand <- performance(pred = pred_ctree_rand, measures = measures)

perform <-data.frame(rpart_default = per_default,
                     rpart_article = per_new_def,
                     rpart_ctrl_grid = per_ctrl,
                     rpart_rand_search = per_rand,
                     ctree_default = per_ctree_default,
                     ctree_ctrl_grid = per_ctree_ctrl,
                     ctree_rand_search = per_ctree_rand) %>% t

# save(perform, file="perform.rda")

perform_infg_gini <- data.frame(Gini = per_new_def_gini,
           Information_Gain = per_new_def_infg) %>% t

# save(perform_infg_gini, file = "perform_infg_gini.rda")
# save(model_new_def_gini, model_new_def_infg, file = "models_infg_gini.rda")


# Plots

library(rpart.plot)

ctrl_plot <- rpart.plot(model_ctrl$learner.model)


rpart.plot(model_new_def_gini$learner.model)
rpart.plot(model_new_def_infg$learner.model)

#save(model_ctrl, model_ctree_ctrl, model_new_def, file="models.rda")

rpart.rules(model_ctrl$learner.model) %>% colnames
rules <- rpart.rules(model_ctrl$learner.model)
colnames(rules)[2:ncol(rules)] <- "-"
index <- sample(1:nrow(rules), 10, replace = FALSE) %>% sort
rules_art <- rpart.rules(model_new_def$learner.model)


library(party)

plot(model_ctree_default$learner.model)
plot(model_ctree_rand$learner.model)
plot(model_ctree_ctrl$learner.model)
















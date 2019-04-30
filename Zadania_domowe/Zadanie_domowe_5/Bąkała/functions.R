require(titanic)
require(missRanger)
require(dplyr)
require(randomsearch)
require(ModelMetrics)
# require(tree)
# require(party)
set.seed(27)

# splitting train ----
set.seed(27)
test_indices <- sample(1:nrow(titanic_train), 200)
titanic_test_ <- titanic_train[test_indices, ]
titanic_train_ <- titanic_train[-test_indices, ]

# preprocessing ----
preprocessing <- function(data) {
  data %>%
    group_by(Ticket) %>%
    mutate(OnTicket = n()) %>%
    ungroup() %>%
    group_by(Cabin) %>%
    mutate(InCabin = ifelse(Cabin == "", 0, n())) %>%
    ungroup() %>%
    dplyr::select(-c(PassengerId, Name, Ticket, Cabin)) %>%
    mutate(Sex = as.factor(Sex), Embarked = as.factor(Embarked)) %>%
    missRanger()
}

titanic_train_imputed <- preprocessing(titanic_train_)
titanic_test_imputed <- preprocessing(titanic_test_)

# support functions ----
obj.fun.ranger.internal <- function(x) {
  model_t <- ranger::ranger(Survived ~ ., titanic_train_imputed, seed = 27, classification = TRUE,
                            num.trees = x["num.trees"], replace = x["replace"], sample.fraction = x["sample.fraction"],
                            mtry = x["mtry"], splitrule = "gini", min.node.size = x["min.node.size"],
                            respect.unordered.factors = x["respect.unordered.factors"])
  pred_t <- predict(model_t, titanic_test_imputed %>% dplyr::select(-Survived))
  mean(titanic_test_imputed$Survived == pred_t$predictions)
}

obj.fun.ranger <- makeSingleObjectiveFunction(
  name = "Optymalne parametry modelu ranger",
  id = "ranger_optimal_hiperparams",
  par.set = makeParamSet(
    makeIntegerParam("num.trees", lower = 1, upper = 2000),
    makeLogicalParam("replace"),
    makeNumericParam("sample.fraction", lower = 0.1, upper = 1),
    makeNumericParam("mtry", lower = 0, upper = 1, trafo = function(x) {x * ncol(titanic_train_imputed)}),
    makeLogicalParam("respect.unordered.factors"),
    makeNumericParam("min.node.size", lower = 0, upper = 1, trafo = function(x) {nrow(titanic_train_imputed)^x})
  ),
  fn = obj.fun.ranger.internal,
  minimize = FALSE
)

obj.fun.rpart.internal <- function(x) {
  model_t <- rpart(Survived ~ ., titanic_train_imputed,
                   cp = x["cp"], maxdepth = x["maxdepth"], minbucket = x["minbucket"], minsplit = x["minsplit"])
  pred_t <- predict(model_t, titanic_test_imputed %>% dplyr::select(-Survived))
  auc(titanic_test_imputed$Survived, pred_t)
}

obj.fun.rpart <- makeSingleObjectiveFunction(
  name = "Optymalne parametry modelu rpart",
  id = "rpart_optimal_hiperparams",
  par.set = makeParamSet(
    makeNumericParam("cp", lower = 0, upper = 1),
    makeIntegerParam("maxdepth", lower = 1, upper = 30),
    makeIntegerParam("minbucket", lower = 1, upper = 60),
    makeIntegerParam("minsplit", lower = 1, upper = 60)
  ),
  fn = obj.fun.rpart.internal,
  minimize = FALSE
)

obj.fun.ctree.internal <- function(x) {
  model_t <- ctree(Survived ~ ., titanic_train_imputed,
                   mincriterion = x["mincriterion"], minbucket = x["minbucket"], minsplit = x["minsplit"])
  pred_t <- predict(model_t, titanic_test_imputed %>% dplyr::select(-Survived))
  auc(titanic_test_imputed$Survived, pred_t)
}

obj.fun.ctree <- makeSingleObjectiveFunction(
  name = "Optymalne parametry modelu ctree",
  id = "ctree_optimal_hiperparams",
  par.set = makeParamSet(
    makeNumericParam("mincriterion", lower = 3, upper = 8, trafo = function(x) 1 - 2^(-x)),
    makeIntegerParam("minbucket", lower = 1, upper = 60),
    makeIntegerParam("minsplit", lower = 1, upper = 60)
  ),
  fn = obj.fun.ctree.internal,
  minimize = FALSE
)

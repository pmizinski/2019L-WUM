source("functions.R")
require(rpart)
require(rpart.plot)
require(rattle)

# training rpart ----
model_p <- rpart(Survived ~ ., titanic_train_imputed)
pred_p <- predict(model_p, titanic_test_imputed %>% dplyr::select(-Survived))

model_o <- rpart(Survived ~ ., titanic_train_imputed,
                 cp = 0, maxdepth = 21, minbucket = 12, minsplit = 24)
pred_o <- predict(model_o, titanic_test_imputed %>% dplyr::select(-Survived))

model_o2 <- rpart(Survived ~ ., titanic_train_imputed, parms = list(split = "information"),
                 cp = 0, maxdepth = 21, minbucket = 12, minsplit = 24)
pred_o2 <- predict(model_o2, titanic_test_imputed %>% dplyr::select(-Survived))

random_search <- randomsearch(obj.fun.rpart)
res <- summary(random_search)
model_t <- rpart(Survived ~ ., titanic_train_imputed,
                 cp = res$best.x$cp, maxdepth = res$best.x$maxdepth,
                 minbucket = res$best.x$minbucket, minsplit = res$best.x$minsplit)
pred_t <- predict(model_t, titanic_test_imputed %>% dplyr::select(-Survived))

# auc ----
auc_p <- auc(titanic_test_imputed$Survived, pred_p)
auc_o <- auc(titanic_test_imputed$Survived, pred_o)
auc_o2 <- auc(titanic_test_imputed$Survived, pred_o2)
auc_t <- auc(titanic_test_imputed$Survived, pred_t)
auc <- data.frame(model = c("default", "article", "information_gain", "tuned"),
                  auc = c(auc_p, auc_o, auc_o2, auc_t))
saveRDS(auc, "auc_rpart.RData")

fancyRpartPlot(model_t, type = 0)

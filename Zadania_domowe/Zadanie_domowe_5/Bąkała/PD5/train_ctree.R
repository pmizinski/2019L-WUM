source("functions.R")
require(partykit)

# training ctree ----
model_p <- ctree(Survived ~ ., titanic_train_imputed)
pred_p <- predict(model_p, titanic_test_imputed %>% dplyr::select(-Survived))

random_search <- randomsearch(obj.fun.ctree)
res <- summary(random_search)
model_t <- ctree(Survived ~ ., titanic_train_imputed,
                 control = ctree_control(mincriterion = 1 - 2^(-res$best.x$mincriterion),
                                         minbucket = res$best.x$minbucket,
                                         minsplit = res$best.x$minsplit))
pred_t <- predict(model_t, titanic_test_imputed %>% dplyr::select(-Survived))

# auc ----
auc_p <- auc(titanic_test_imputed$Survived, pred_p)
auc_t <- auc(titanic_test_imputed$Survived, pred_t)
auc <- data.frame(model = c("default", "tuned"),
                  auc = c(auc_p, auc_t))
saveRDS(auc, "auc_ctree.RData")

plot(model_t)

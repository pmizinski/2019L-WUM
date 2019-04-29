source("functions.R")
require(ranger)

# training ranger ----
model_p <- ranger::ranger(Survived ~ ., titanic_train_imputed, seed = 27, classification = TRUE, probability = TRUE)
pred_p <- predict(model_p, titanic_test_imputed %>% dplyr::select(-Survived))

model_o <- ranger::ranger(Survived ~ ., titanic_train_imputed, seed = 27, classification = TRUE, probability = TRUE,
                          num.trees = 983, replace = FALSE, sample.fraction = 0.703,
                          mtry = ncol(titanic_train_imputed) * 0.257, splitrule = "gini",
                          respect.unordered.factors = FALSE, min.node.size = 1)
pred_o <- predict(model_o, titanic_test_imputed %>% dplyr::select(-Survived))

random_search <- randomsearch(obj.fun.ranger)
res <- summary(random_search)
model_t <- ranger::ranger(Survived ~ ., titanic_train_imputed, seed = 27, classification = TRUE, probability = TRUE,
                          num.trees = res$best.x$num.trees,
                          replace = res$best.x$replace,
                          sample.fraction = res$best.x$sample.fraction,
                          mtry = res$best.x$mtry * ncol(titanic_train_imputed),
                          splitrule = "gini",
                          min.node.size = nrow(titanic_train_imputed)^res$best.x$min.node.size,
                          respect.unordered.factors = res$best.x$respect.unordered.factors)
pred_t <- predict(model_t, titanic_test_imputed %>% dplyr::select(-Survived))

# auc ----
auc_p <- auc(titanic_test_imputed$Survived, pred_p$predictions)
auc_o <- auc(titanic_test_imputed$Survived, pred_o$predictions)
auc_t <- auc(titanic_test_imputed$Survived, pred_t$predictions)
auc <- data.frame(model = c("default", "article", "tuned"),
                  auc = c(auc_p, auc_o, auc_t))
saveRDS(auc, "auc_ranger.RData")

model_t_no_prob <- ranger::ranger(Survived ~ ., titanic_train_imputed, seed = 27, classification = TRUE)
model_tree <- treeInfo(model_t_no_prob)
saveRDS(model_tree, "model_tree_ranger.RData")

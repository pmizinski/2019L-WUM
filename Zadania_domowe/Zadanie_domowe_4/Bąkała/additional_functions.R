######################################################## apartments
regr_function <- function(gamma, cost) {
  require("e1071")
  require("MLmetrics")
  gamma <- exp(gamma)
  cost <- exp(cost)
  x_model <- svm(formula(m2.price ~ .), apartments,
                 gamma = gamma,
                 cost = cost)
  x_pred <- predict(x_model, apartmentsTest)
  x_rmse <- -RMSE(x_pred, apartmentsTest$m2.price)
  list(Score = x_rmse,
       Pred = x_pred)
}

######################################################## image-seg
classif_function <- function(gamma, cost) {
  require("e1071")
  require("MLmetrics")
  gamma <- exp(gamma)
  cost <- exp(cost)
  x_model <- svm(formula(pixel.class ~ .), image_seg,
                 gamma = gamma,
                 cost = cost)
  x_pred <- predict(x_model, image_seg_test)
  x_acc <- Accuracy(x_pred, image_seg_test$pixel.class)
  list(Score = x_acc,
       Pred = x_pred)
}

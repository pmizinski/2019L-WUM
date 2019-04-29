library(DALEX)
library(dplyr)
library(mlr)

abalone <- read.csv("winequality-red.csv")

summary(abalone)
summary(apartments)

########################
####    Zadanie 1   ####
########################

# Tasks

task_ab <- makeRegrTask(data = abalone, target = "quality")
task_ap <- makeRegrTask(id = "task", data = apartments, target = "m2.price")

# Learners

lrn_ab <- makeLearner("regr.svm", par.vals = list(scale = FALSE, kernel = "radial"))
lrn_ap <- makeLearner("regr.svm", par.vals = list(scale = FALSE, kernel = "radial"))

## Dodatkowe modele drzewiaste:

lrn_T_ab <- makeLearner("regr.randomForest")
lrn_T_ap <- makeLearner("regr.randomForest")


# Przygotowanie resamplingu

rdesc <- makeResampleDesc("CV", iters = 5)

# Sprawdzanie modeli
measures <- list(mse, rmse, mae, rsq)

cv_ap <- resample(lrn_ap, task_ap, rdesc, measures = measures)

cv_T_ap <- resample(lrn_T_ap, task_ap, rdesc, measures = measures)

cv_ab <- resample(lrn_ab, task_ab, rdesc, measures = measures)

cv_T_ab <- resample(lrn_T_ab, task_ab, rdesc, measures = measures)

########################
####    Zadanie 2   ####
########################

norm_cv_ap <- resample(lrn_ap, normalizeFeatures(task_ap), rdesc, measures = measures)

norm_cv_T_ap <- resample(lrn_T_ap, normalizeFeatures(task_ap), rdesc, measures = measures)

norm_cv_ab <- resample(lrn_ab, normalizeFeatures(task_ab), rdesc, measures = measures)

norm_cv_T_ab <- resample(lrn_T_ab, normalizeFeatures(task_ab), rdesc, measures = measures)


cv_ap
norm_cv_ap

cv_T_ap
norm_cv_T_ap

cv_ab
norm_cv_ab

cv_T_ab
norm_cv_T_ab

# Konkluzja: rzeczywiœcie normalizacja odrobinê poprawia predykcjê

########################
####    Zadanie 3   ####
########################

# Najwa¿niejsze parametry to:
#  *  C - szerokoœæ ulicy
#  *  stopieñ kernela - przy kernelu wielomianowym
#  *  gamma - przy kernelu gausowskim
 
# Bêdziemy rozwa¿aæ j¹dro gausowskie

# Parametry
parset <- makeParamSet(
  makeNumericParam("gamma", lower = -5, upper = 5, trafo = function(x) 2^x)
)

# Liczba iteracji
ctrl <-  makeTuneControlRandom(maxit = 50L)

# Tuning

res_ap <- tuneParams("regr.svm",
                  task = task_ap,
                  control = ctrl,
                  measures = measures,
                  resampling = rdesc,
                  par.set = parset,
                  show.info = TRUE)

res_ap2 <- generateHyperParsEffectData(res_ap, trafo = T, include.diagnostics = FALSE)

res_ab <- tuneParams("regr.svm",
                     task = task_ab,
                     control = ctrl,
                     measures = measures,
                     resampling = rdesc,
                     par.set = parset,
                     show.info = TRUE)

res_ab2 <- generateHyperParsEffectData(res_ab, trafo = T, include.diagnostics = FALSE)

save(res_ap, file = "res_ap")
save(res_ab, file = "res_ab")
save(res_ap2, file = "res_ap2")

save(res_ab2, file = "res_ab2")

# Ok, znalezione zosta³y hiperparametry

########################
####    Zadanie 4   ####
########################

set.seed(123)

# Modele

sam <- sample(1:nrow(abalone), nrow(abalone)/5)
abalone_test <- abalone[sam,]
abalone_train <- abalone[-sam,]
  
task_ab <- makeRegrTask(data = abalone_train, target = "quality")

model_ab <- train(lrn_ab, task_ab) 
model_ap <- train(lrn_ap, task_ap)

model_T_ab <- train(lrn_T_ab, task_ab) 
model_T_ap <- train(lrn_T_ap, task_ap)


lrn_ab_tuned <- makeLearner("regr.svm", par.vals = list(gamma = 0.204))
lrn_ap_tuned <- makeLearner("regr.svm", par.vals = list(gamma = 0.0451))

model_ab_tuned <- train(lrn_ab_tuned, task_ab)
model_ap_tuned <- train(lrn_ap_tuned, task_ap)

# Explainers 

custom_predict_regr <- function(object, newdata) {
  pred <- predict(object, newdata=newdata)
  response <- pred$data$response
  return(response)
}


explainer_ab <- explain(model_ab,
                        data = abalone_test,
                        y = abalone_test$quality,
                        predict_function = custom_predict_regr,
                        label="default_svm")

explainer_ap <- explain(model_ap,
                        data = apartmentsTest,
                        y=apartmentsTest$m2.price,
                        predict_function = custom_predict_regr,
                        label="default_svm")

explainer_T_ab <- explain(model_T_ab,
                        data = abalone_test,
                        y = abalone_test$quality,
                        predict_function = custom_predict_regr,
                        label="rf")

explainer_T_ap <- explain(model_T_ap,
                        data = apartmentsTest,
                        y=apartmentsTest$m2.price,
                        predict_function = custom_predict_regr,
                        label="rf")

explainer_ab_tuned <- explain(model_ab_tuned,
                        data = abalone_test,
                        y = abalone_test$quality,
                        predict_function = custom_predict_regr,
                        label="tuned_svm")

explainer_ap_tuned <- explain(model_ap_tuned,
                        data = apartmentsTest,
                        y=apartmentsTest$m2.price,
                        predict_function = custom_predict_regr,
                        label="tuned_svm")

# Model Performance

mp_ab <- model_performance(explainer_ab)
mp_ap <- model_performance(explainer_ap)
mp_T_ab <- model_performance(explainer_T_ab)
mp_T_ap <- model_performance(explainer_T_ap)
mp_ab_tuned <- model_performance(explainer_ab_tuned)
mp_ap_tuned <- model_performance(explainer_ap_tuned)

plot_ab_perfor <- plot(mp_ab, mp_ab_tuned, mp_T_ab)
plot_ap_perfor <- plot(mp_ap, mp_ap_tuned, mp_T_ap)

save(plot_ab_perfor,
     plot_ap_perfor,
     file='plots3.rda')
# PDP

# ap
pdp_ap_surface  <- variable_response(
  explainer_ap,
  variable =  "surface",
  type = "pdp"
)
pdp_ap_district  <- variable_response(
  explainer_ap,
  variable =  "district",
  type = "factor"
)
pdp_ap_floor  <- variable_response(
  explainer_ap,
  variable =  "floor",
  type = "pdp"
)
pdp_ap_con.year  <- variable_response(
  explainer_ap,
  variable =  "construction.year",
  type = "pdp"
)
pdp_ap_no.rooms  <- variable_response(
  explainer_ap,
  variable =  "no.rooms",
  type = "pdp"
)

# T_ap
pdp_T_ap_surface  <- variable_response(
  explainer_T_ap,
  variable =  "surface",
  type = "pdp"
)
pdp_T_ap_district  <- variable_response(
  explainer_T_ap,
  variable =  "district",
  type = "factor"
)
pdp_T_ap_floor  <- variable_response(
  explainer_T_ap,
  variable =  "floor",
  type = "pdp"
)
pdp_T_ap_con.year  <- variable_response(
  explainer_T_ap,
  variable =  "construction.year",
  type = "pdp"
)
pdp_T_ap_no.rooms  <- variable_response(
  explainer_T_ap,
  variable =  "no.rooms",
  type = "pdp"
)

# ap_tuned

pdp_ap_tuned_surface  <- variable_response(
  explainer_ap_tuned,
  variable =  "surface",
  type = "pdp"
)
pdp_ap_tuned_district  <- variable_response(
  explainer_ap_tuned,
  variable =  "district",
  type = "factor"
)
pdp_ap_tuned_floor  <- variable_response(
  explainer_ap_tuned,
  variable =  "floor",
  type = "pdp"
)
pdp_ap_tuned_con.year  <- variable_response(
  explainer_ap_tuned,
  variable =  "construction.year",
  type = "pdp"
)
pdp_ap_tuned_no.rooms  <- variable_response(
  explainer_ap_tuned,
  variable =  "no.rooms",
  type = "pdp"
)

plot_surface <- plot(pdp_ap_tuned_surface, pdp_ap_surface, pdp_T_ap_surface)
plot_district <- plot(pdp_ap_tuned_district, pdp_ap_district, pdp_T_ap_district)
plot_floor <- plot(pdp_ap_tuned_floor, pdp_ap_floor, pdp_T_ap_floor)
plot_con.year <- plot(pdp_ap_tuned_con.year, pdp_ap_con.year, pdp_T_ap_con.year)
plot_no.rooms <- plot(pdp_ap_tuned_no.rooms, pdp_ap_no.rooms, pdp_T_ap_no.rooms)

save(plot_surface,
     plot_district,
     plot_floor,
     plot_con.year,
     plot_no.rooms,
     file = "plots.rda")

# ab

pdp_ab_alcohol  <- variable_response(
  explainer_ab,
  variable =  "alcohol",
  type = "pdp"
)
pdp_ab_sulphates  <- variable_response(
  explainer_ab,
  variable =  "sulphates",
  type = "pdp"
)
pdp_ab_total.sulfur.dioxide  <- variable_response(
  explainer_ab,
  variable =  "total.sulfur.dioxide",
  type = "pdp"
)
pdp_ab_volatile.acidity  <- variable_response(
  explainer_ab,
  variable =  "volatile.acidity",
  type = "pdp"
)
pdp_ab_pH  <- variable_response(
  explainer_ab,
  variable =  "pH",
  type = "pdp"
)
##
pdp_T_ab_alcohol  <- variable_response(
  explainer_T_ab,
  variable =  "alcohol",
  type = "pdp"
)
pdp_T_ab_sulphates  <- variable_response(
  explainer_T_ab,
  variable =  "sulphates",
  type = "pdp"
)
pdp_T_ab_total.sulfur.dioxide  <- variable_response(
  explainer_T_ab,
  variable =  "total.sulfur.dioxide",
  type = "pdp"
)
pdp_T_ab_volatile.acidity  <- variable_response(
  explainer_T_ab,
  variable =  "volatile.acidity",
  type = "pdp"
)
pdp_T_ab_pH  <- variable_response(
  explainer_T_ab,
  variable =  "pH",
  type = "pdp"
)
##
pdp_ab_tuned_alcohol  <- variable_response(
  explainer_ab_tuned,
  variable =  "alcohol",
  type = "pdp"
)
pdp_ab_tuned_sulphates  <- variable_response(
  explainer_ab_tuned,
  variable =  "sulphates",
  type = "pdp"
)
pdp_ab_tuned_total.sulfur.dioxide  <- variable_response(
  explainer_ab_tuned,
  variable =  "total.sulfur.dioxide",
  type = "pdp"
)
pdp_ab_tuned_volatile.acidity  <- variable_response(
  explainer_ab_tuned,
  variable =  "volatile.acidity",
  type = "pdp"
)
pdp_ab_tuned_pH  <- variable_response(
  explainer_ab_tuned,
  variable =  "pH",
  type = "pdp"
)

plot_alcohol <- plot(pdp_ab_tuned_alcohol, pdp_ab_alcohol, pdp_T_ab_alcohol)
plot_sulphates <- plot(pdp_ab_tuned_sulphates, pdp_ab_sulphates, pdp_T_ab_sulphates)
plot_total.sulfur.dioxide <- plot(pdp_ab_tuned_total.sulfur.dioxide, pdp_ab_total.sulfur.dioxide, pdp_T_ab_total.sulfur.dioxide)
plot_volatile.acidity <- plot(pdp_ab_tuned_volatile.acidity, pdp_ab_volatile.acidity, pdp_T_ab_volatile.acidity)
plot_pH <- plot(pdp_ab_tuned_pH, pdp_ab_pH, pdp_T_ab_pH)

save(plot_alcohol,
     plot_sulphates,
     plot_total.sulfur.dioxide,
     plot_volatile.acidity,
     plot_pH,
     file = "plots_wine.rda")

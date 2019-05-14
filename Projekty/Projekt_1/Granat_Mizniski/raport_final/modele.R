library(dplyr)
library(mlr)

dane <- read.csv("./compass/cox-violent-parsed.csv",na.strings=c(""," ","NA"))

###Przygotowanie danych

##usuniecie zbednych kolumn
dane <- dane[,2:25]
drops<-c("decile_score","compas_screening_date","days_b_screening_arrest","c_days_from_compas")
dane <- dane[,!(names(dane) %in% drops)]
dane <- unique(dane)
dane <- dane %>% filter(is_recid %in% c(0,1))

##modyfikacja kolumn
#dlugosc odsiadki **
dane$c_jail_in <- as.POSIXct(dane$c_jail_in, format = "%d/%m/%Y")
dane$c_jail_out <- as.POSIXct(dane$c_jail_out, format = "%d/%m/%Y")
dane <- dane %>% mutate(jail_long = difftime( c_jail_out,c_jail_in, units = "days"))
#grupa dlugosc odsiadki
#dane$grLong = ifelse(dane$jail_long > 7,
#                  ifelse(dane$jail_long > 14,
#                         ifelse(dane$jail_long > 30,
#                                ifelse(dane$jail_long > 180,
#                                       ifelse(dane$jail_long > 360,5,4),3),2),1),0)
dane$grLong = ifelse(dane$jail_long > 7,
                     ifelse(dane$jail_long > 14,
                            ifelse(dane$jail_long > 30,
                                   ifelse(dane$jail_long > 180,
                                          ifelse(dane$jail_long > 360,">360","180-360"),"30-180"),"14-30"),"7-14"),"<7")

dane$chargeGr = ifelse(dane$c_charge_degree == "(F1)" | dane$c_charge_degree == "(F2)", "a",
                      ifelse(dane$c_charge_degree == "(F3)" | dane$c_charge_degree == "(M1)" | dane$c_charge_degree == "(F7)","b",
                            ifelse(dane$c_charge_degree == "(M2)" | dane$c_charge_degree == "(M03)","c","d")))

dane$juv_fel_count = ifelse(dane$juv_fel_count > 0,
                            ifelse(dane$juv_fel_count > 1,">1","1"),"0")

dane$juv_misd_count = ifelse(dane$juv_misd_count > 0,
                            ifelse(dane$juv_misd_count > 1,">1","1"),"0")

dane$juv_other_count = ifelse(dane$juv_other_count > 0,
                             ifelse(dane$juv_other_count > 1,">1","1"),"0")

dane$priors_count = ifelse(dane$priors_count > 0,
                              ifelse(dane$priors_count > 1,
                                     ifelse(dane$priors_count > 3,
                                            ifelse(dane$priors_count > 6,">6","4-6"),"2-3"),"1"),"0")

#podzial na zbior treningowy i testowy
n <- nrow(dane)
ind <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.8, 0.2))
daneTr <- dane[ind, ]
daneTest <- dane[!ind, ]

## modele

#wybor kolumn, ktore uzyjemy do modeli itp
daneTrv1 <- daneTr[,c(4,7,8,9,10,11,12,20,22,23)]

#zamiana na factory i uzupelnianie missing values
daneTrv1$is_recid <- factor(daneTrv1$is_recid)
daneTrv1$grLong <- factor(daneTrv1$grLong)
daneTrv1$chargeGr <- factor(daneTrv1$chargeGr)
daneTrv1$juv_fel_count <- factor(daneTrv1$juv_fel_count)
daneTrv1$juv_misd_count <- factor(daneTrv1$juv_misd_count)
daneTrv1$juv_other_count <- factor(daneTrv1$juv_other_count)
daneTrv1$priors_count <- factor(daneTrv1$priors_count)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

daneTrv1 <- daneTrv1 %>% mutate_if(is.numeric, funs(replace(.,is.na(.), mean(., na.rm = TRUE)))) %>%
  mutate_if(is.factor, funs(replace(.,is.na(.), Mode(na.omit(.)))))

#task i learner dla 3 podstawowych modeli
classif_task <- makeClassifTask(id="classif", data = daneTrv1, target = "is_recid")

classif_lrn <- makeLearner("classif.logreg", predict.type = "prob")
classif_lrn2 <- makeLearner("classif.kknn", predict.type = "prob")
classif_lrn3 <- makeLearner("classif.svm", predict.type = "prob")

#trenujemy
mod <- train(classif_lrn,classif_task)
mod2 <- train(classif_lrn2,classif_task)
mod3 <- train(classif_lrn3,classif_task)

#factory i uzupelnienie dla testowego zbioru, zeby byl jak treningowy
daneTest <- daneTest[,c(4,7,8,9,10,11,12,20,22,23)]

daneTest$is_recid <- factor(daneTest$is_recid)
daneTest$grLong <- factor(daneTest$grLong)
daneTest$chargeGr <- factor(daneTest$chargeGr)
daneTest$juv_fel_count <- factor(daneTest$juv_fel_count)
daneTest$juv_misd_count <- factor(daneTest$juv_misd_count)
daneTest$juv_other_count <- factor(daneTest$juv_other_count)
daneTest$priors_count <- factor(daneTest$priors_count)

daneTest <- daneTest %>% mutate_if(is.numeric, funs(replace(.,is.na(.), mean(., na.rm = TRUE)))) %>%
  mutate_if(is.factor, funs(replace(.,is.na(.), Mode(na.omit(.)))))

#przewidujemy dla zbioru testowego
pred <- predict(mod, newdata = daneTest)
head(as.data.frame(pred))
performance(pred,measures = acc)

pred2 <- predict(mod2, newdata = daneTest)
head(as.data.frame(pred2))
performance(pred2,measures = acc)

pred3 <- predict(mod3, newdata = daneTest)
head(as.data.frame(pred3))
performance(pred3,measures = acc)

##association rules, czyli biblioteka arules, szykamy jakis powiazan
library(arules)
#library(arulesViz)

trans <- as(daneTrv1, "transactions")
trans
summary(trans)
itemLabels(trans)

#powiazania dla is_recid = 1
rules <- apriori (data=trans, parameter=list (supp=0.001,conf = 0.08, minlen = 3), 
                  appearance = list (default="lhs",rhs="is_recid=1"), control = list (verbose=F))
rules_conf <- sort (rules, by="lift", decreasing=TRUE) # 'high-confidence' rules.

inspect(head(rules_conf,10))

daneTrv1 %>% filter(age_cat=="Less than 25" & juv_misd_count== ">1" & juv_other_count=="1") # <------ wynik do prezki
#powiazania dla is_recid = 0
rules2 <- apriori (data=trans, parameter=list (supp=0.001,conf = 0.08, minlen = 3), 
                  appearance = list (default="lhs",rhs="is_recid=0"), control = list (verbose=F))
rules_conf2 <- sort (rules2, by="lift", decreasing=TRUE) # 'high-confidence' rules.

inspect(head(rules_conf2,10))



###modele i ich wizualizacje v2, tutaj sobie wybiearamy ktore kolumny chcemy wybrac, obecnie dla calego wybranego wczesniej zbioru

#task i learner
daneTrv2 <- daneTrv1[,c(1,2,3,4,5,6,7,8,9,10)]
daneTest2 <- daneTest[,c(1,2,3,4,5,6,7,8,9,10)]

classif_task2 <- makeClassifTask(id="classif", data = daneTrv2, target = "is_recid")

#trenujemy
mod <- train(classif_lrn,classif_task2)
mod2 <- train(classif_lrn2,classif_task2)
mod3 <- train(classif_lrn3,classif_task2)

pred <- predict(mod, newdata = daneTest2)
head(as.data.frame(pred))
performance(pred,measures = acc)

pred2 <- predict(mod2, newdata = daneTest2)
head(as.data.frame(pred2))
performance(pred2,measures = acc)

pred3 <- predict(mod3, newdata = daneTest2)
head(as.data.frame(pred3))
performance(pred3,measures = acc)

##wizualizacje dalex
library(DALEX)
library(auditor)

y_test <- as.numeric(as.character(daneTest2$is_recid))

custom_predict_classif <- function(object, newdata) {pred <- predict(object, newdata=newdata)
response <- pred$data[,3]
return(response)}

explainer_classif_1 <- DALEX::explain(mod, data=daneTest2, y=y_test, label= "logreg", predict_function = custom_predict_classif)
explainer_classif_2 <- DALEX::explain(mod2, data=daneTest2, y=y_test, label= "knn", predict_function = custom_predict_classif)
explainer_classif_3 <- DALEX::explain(mod3, data=daneTest2, y=y_test, label= "svm", predict_function = custom_predict_classif)

# waznosc zmiennych
vi_classif_1 <- variable_importance(explainer_classif_1, loss_function = loss_root_mean_square, type="difference")
vi_classif_2 <- variable_importance(explainer_classif_2, loss_function = loss_root_mean_square, type="difference")
vi_classif_3 <- variable_importance(explainer_classif_3, loss_function = loss_root_mean_square, type="difference")

plot1 <- plot(vi_classif_1)
plot2 <- plot(vi_classif_2)
plot3 <- plot(vi_classif_3)

# wykres residuow
mp_classif_1 <- model_performance(explainer_classif_1)
mp_classif_2 <- model_performance(explainer_classif_2)
mp_classif_3 <- model_performance(explainer_classif_3)

plot4 <- plot(mp_classif_1, mp_classif_2, mp_classif_3)


# #################################### brudnopis
# 
# itemFrequencyPlot(trans, topN=50,  cex.names=0.5)
# 
# trans_recid <- subset(trans, items %in% "is_recid=1")
# itemFrequencyPlot(trans_recid, topN = 25, population = trans, cex.names=.5) #ten cos czaje chyba
# 
# itemFrequencyPlot(trans_female, topN = 25, population = trans, lift=TRUE, cex.names=.5)
# 
# itemsets <- apriori(trans, parameter = list(target = "frequent", supp=0.1, minlen = 2, maxlen=4))
# inspect(head(sort(itemsets), n=20))
# 
# quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = trans)
# inspect(head(sort(itemsets, by = "lift"), n=10))
# plot(head(sort(itemsets, by = "lift"), n=50), method = "graph", control=list(cex=.8))
# 
# #
# frequentItems <- eclat (trans, parameter = list(supp = 0.07, minlen = 5))
# inspect(head(sort(frequentItems, by = "support"), n=10))
# itemFrequencyPlot(trans, topN=10, type="absolute", main="Item Frequency")
# 
# rules <- apriori (trans, parameter = list(supp = 0.001, conf = 0.5))
# subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
# length(subsetRules)  #> 3913
# rules <- rules[-subsetRules]
# 
# rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
# inspect(head(rules_conf,20))
# 
# rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
# inspect(head(rules_lift,20))
# #
# 
# library(arulesCBA)
# 
# set.disc <- discretizeDF.supervised(is_recid ~., daneTrv1)
# trans <- as(set.disc, "transactions")
# truth <- set.disc$is_recid
# cars <- mineCARs(is_recid ~ ., trans, parameter = list(support = .01, confidence = .3))
# cars <- cars[!is.redundant(cars)]
# cars <- sort(cars, by = "conf")
# # create classifier
# cl <- CBA_ruleset(is_recid ~ ., cars)
# cl
# # look at the rule base
# rules(cl)
# # make predictions
# prediction <- predict(cl, daneTest)
# table(prediction, daneTest$is_recid)
# head(as.data.frame(prediction))
# performance(prediction,measures = acc)
# 
# daneTest <- daneTest[,c(1,2,3,8,9,10)]

#########################

daneTrv3 <- daneTrv1[,c(7,8)]
daneTrv4 <- daneTrv1[,c(7,8,10)]
daneTrv5 <- daneTrv1[,c(1,3,7,8)]

classif_task1 <- makeClassifTask(id="classif", data = daneTrv1, target = "is_recid")
classif_task2 <- makeClassifTask(id="classif2", data = daneTrv2, target = "is_recid")
classif_task3 <- makeClassifTask(id="classif3", data = daneTrv3, target = "is_recid")
classif_task4 <- makeClassifTask(id="classif4", data = daneTrv4, target = "is_recid")
classif_task5 <- makeClassifTask(id="classif5", data = daneTrv5, target = "is_recid")

lrns = list(makeLearner("classif.rpart", predict.type="prob"), makeLearner("classif.naiveBayes", predict.type = "prob"), makeLearner("classif.svm", predict.type = "prob"))
tasks = list(classif_task1,classif_task2,classif_task3,classif_task4,classif_task5)
rdesc = makeResampleDesc("Holdout")
meas = list(acc,auc)
bmr = benchmark(lrns, tasks, rdesc, measures = meas)
bmr

tasks2 = list(classif_task1)
lrns2 = list(makeLearner("classif.rpart", predict.type="prob", id = "rpart1"), makeLearner("classif.rpart", predict.type="prob", maxdepth = 5,id = "rpart2"), makeLearner("classif.rpart", predict.type="prob", maxdepth = 10, id = "rpart3"), makeLearner("classif.rpart", predict.type="prob", maxdepth = 20))
bmr2 = benchmark(lrns2, tasks2, rdesc, measures = meas)

bmr2

tasks3 = list(classif_task1)
lrns3 = list(makeLearner("classif.randomForest", predict.type="prob", ntree = 10,id = "rfor10"), makeLearner("classif.randomForest", predict.type="prob", ntree = 50,id = "rfor50"),makeLearner("classif.randomForest", predict.type="prob", ntree = 100,id = "rfor100"),makeLearner("classif.randomForest", predict.type="prob", ntree = 500,id = "rfor500"),makeLearner("classif.randomForest", predict.type="prob", ntree = 1000,id = "rfor1000"))
bmr3 = benchmark(lrns3, tasks3, rdesc, measures = meas) 
bmr3

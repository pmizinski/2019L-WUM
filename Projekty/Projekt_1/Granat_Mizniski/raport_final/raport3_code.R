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
set.seed(123)
n <- nrow(dane)
ind <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.8, 0.2))
daneTr <- dane[ind, ]
daneTest <- dane[!ind, ]

## modele

#wybor kolumn, ktore uzyjemy do modeli itp
daneTrv1 <- daneTr[,c(4,7,8,9,10,11,12,20,22,23)]

#zamiana na factory i uzupelnianie missing values # <------ dodac do prezki, ze uzupelnianie na moda 
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

classif_task <- makeClassifTask(id="classif", data = daneTrv1, target = "is_recid")

classif_lrn <- makeLearner("classif.logreg", predict.type = "prob")

mod <- train(classif_lrn,classif_task)

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
performance(pred,measures = acc) # <------ wynik do prezki


###tuning hiperparametrow

rdesc = makeResampleDesc("CV", iters = 5L)


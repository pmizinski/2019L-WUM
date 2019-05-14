library(dplyr)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(scales)
library(DataExplorer)
library(corrplot)
library(minerva)
library(multipanelfigure)
dane <- read.csv("./compass/cox-violent-parsed.csv",na.strings=c("","NA"))

###Przygotowanie danych

##usuniecie zbednych kolumn
dane <- dane[,2:25]
drops<-c("decile_score","compas_screening_date","days_b_screening_arrest","c_days_from_compas","name","first","last")
dane <- dane[,!(names(dane) %in% drops)]
dane <- unique(dane)
dane <- dane %>% filter(is_recid %in% c(0,1))

df1 <- dane

drops<-c("c_arrest_date")
dane <- dane[,!(names(dane) %in% drops)]



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
#tabelka dlugosc odsiadki **
dane %>% group_by(grLong)%>% summarise(n=n())

#podzial na zbior treningowy i testowy
n <- nrow(dane)
ind <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.8, 0.2))
daneTr <- dane[ind, ]
daneTest <- dane[!ind, ]

df2 <- daneTr

### Wykresy

##Rasa
#liczba
daneRasa <- daneTr %>%
    select(race, is_recid) %>%
    group_by(race) %>%
    summarize(yes_recid = sum(is_recid == 1), no_recid = sum(is_recid == 0))

daneRasaMelt <- melt(daneRasa,id.vars=c("race"))
daneRasaMelt$race <- factor(daneRasaMelt$race, levels = daneRasaMelt$race[order(daneRasaMelt$value[1:6],decreasing = TRUE)])

p1_1 <- ggplot(daneRasaMelt,aes(x=race,y=value,fill=variable))+
  geom_bar(stat = "identity") +
  xlab("Race") +
  ylab("Number of prisoners") +
  guides(fill = FALSE) 

#procenty
daneRasaP <- daneRasa %>% mutate(sum = yes_recid+no_recid, yes = yes_recid/sum * 100, no =no_recid/sum * 100 )
daneRasaP <- daneRasaP[,c(1,5,6)]

daneRasaPMelt <- melt(daneRasaP,id.vars=c("race"))
daneRasaPMelt$race <- factor(daneRasaPMelt$race, levels = daneRasaPMelt$race[order(daneRasaMelt$value[1:6],decreasing = TRUE)])

p1_2 <- ggplot(daneRasaPMelt,aes(x=race,y=value,fill=variable))+
  geom_bar(stat = "identity") +
  xlab("Race") +
  ylab("Percentage of prisoners") +
  guides(fill = FALSE) 

##DLugosc odsiadki**
#liczba**
daneOdsiadka <- daneTr %>%
  select(grLong, is_recid) %>%
  group_by(grLong) %>%
  summarize(yes_recid = sum(is_recid == 1), no_recid = sum(is_recid == 0))

daneOdsiadkaMelt <- melt(daneOdsiadka,id.vars=c("grLong"))
daneOdsiadkaMelt$grLong <- factor(daneOdsiadkaMelt$grLong, levels = c("<7","7-14","14-30","30-180","180-360",">360"))

p2_1 <- ggplot(daneOdsiadkaMelt,aes(x=grLong,y=value,fill=variable))+
  geom_bar(stat = "identity") +
  xlab("Time in jail") +
  ylab("Number of prisoners") +
  guides(fill = FALSE) 

#procenty**
daneOdsiadkaP <- daneOdsiadka %>% mutate(sum = yes_recid+no_recid, yes = yes_recid/sum * 100, no =no_recid/sum * 100 )
daneOdsiadkaP <- daneOdsiadkaP[,c(1,5,6)]

daneOdsiadkaPMelt <- melt(daneOdsiadkaP,id.vars=c("grLong"))
daneOdsiadkaPMelt$grLong <- factor(daneOdsiadkaPMelt$grLong, levels = c("<7","7-14","14-30","30-180","180-360",">360"))

p2_2 <- ggplot(daneOdsiadkaPMelt,aes(x=grLong,y=value,fill=variable))+
  geom_bar(stat = "identity") +
  xlab("Time in jail") +
  ylab("Percentage of prisoners") +
  guides(fill = FALSE) 

##grupa wiekowa**
#liczba
daneWiek <- daneTr %>%
  select(age_cat, is_recid) %>%
  group_by(age_cat) %>%
  summarize(yes_recid = sum(is_recid == 1), no_recid = sum(is_recid == 0))

daneWiekMelt <- melt(daneWiek,id.vars=c("age_cat"))
daneWiekMelt$age_cat <- factor(daneWiekMelt$age_cat, levels = c("Less than 25","25 - 45","Greater than 45"))

p3_1 <- ggplot(daneWiekMelt,aes(x=age_cat,y=value,fill=variable))+
  geom_bar(stat = "identity") +
  xlab("Age") +
  ylab("Number of prisoners") +
  guides(fill = FALSE) 

#procent
daneWiekP <- daneWiek %>% mutate(sum = yes_recid+no_recid, yes = yes_recid/sum * 100, no =no_recid/sum * 100 )
daneWiekP <- daneWiekP[,c(1,5,6)]

daneWiekPMelt <- melt(daneWiekP,id.vars=c("age_cat"))
daneWiekPMelt$age_cat <- factor(daneWiekPMelt$age_cat, levels = c("Less than 25","25 - 45","Greater than 45"))

p3_2 <- ggplot(daneWiekPMelt,aes(x=age_cat,y=value,fill=variable))+
  geom_bar(stat = "identity") +
  xlab("Age") +
  ylab("Percentage of prisoners") +
  guides(fill = FALSE) 

## crime degree**
# https://www.davidcohenlawfirm.com/pennsylvania-crime-classification
#- Murder
#- Felony (1st degree) (F1) -> przestepstwo
#- Felony (2nd degree) (F2)
#- Felony (3rd degree) (F3)
#- Ungraded Felony (F3)
#- Misdemeanor (1st degree)(M1) -> wykroczenie
#- Misdemeanor (2nd degree)(M2)
#- Misdemeanor (3rd degree)(M3)
#- Ungraded Misdemeanor (Same as M3)
#- Summary Offenses

#liczba
daneCD <- daneTr %>%
  select(c_charge_degree, is_recid) %>%
  group_by(c_charge_degree) %>%
  summarize(yes_recid = sum(is_recid == 1), no_recid = sum(is_recid == 0))

daneCDMelt <- melt(daneCD,id.vars=c("c_charge_degree"))

p4_1 <- ggplot(daneCDMelt,aes(x=c_charge_degree,y=value,fill=variable))+
  geom_bar(stat = "identity") +
  xlab("Grade of crime") +
  ylab("Number of prisoners") +
  guides(fill = FALSE) 

#procent
daneCDP <- daneCD %>% mutate(sum = yes_recid+no_recid, yes = yes_recid/sum * 100, no =no_recid/sum * 100 )
daneCDP <- daneCDP[,c(1,5,6)]

daneCDPMelt <- melt(daneCDP,id.vars=c("c_charge_degree"))

p4_2 <- ggplot(daneCDPMelt,aes(x=c_charge_degree,y=value,fill=variable))+
  geom_bar(stat = "identity") +
  xlab("Grade of crime") +
  ylab("Percentage of prisoners") +
  guides(fill = FALSE) 


df3 <- daneTr[c("is_recid","sex", "age", "age_cat","race","juv_fel_count","juv_misd_count","juv_other_count","priors_count","c_charge_degree","grLong","jail_long")]
p5 <- DataExplorer::plot_correlation(df3)

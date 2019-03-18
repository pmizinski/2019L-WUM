library(ggplot2)
library(DataExplorer)
library(dplyr)
library(dataMaid)
library(lubridate)

dat <- read.csv("allegro-api-transactions.csv", stringsAsFactors = FALSE)
names <- read.csv("allegro-categories-eng.txt", sep=";")
dat <- dplyr::left_join(dat, names, by="main_category")

date_dat <- data.frame(cbind(dat$date[order(dat$date)], rep(1, times = length(dat$date))))

ready_dates <- date_dat %>% group_by(X1) %>% count()
ready_dates$n <- cumsum(as.numeric(ready_dates$n))

#cities <- dat$it_location[!is.na(dat$it_location)]
cities <- dat$it_location
cities <- table(cities)
ready_cities <- data.frame(cities[order(cities, decreasing = TRUE)][1:10])

cities_part <- sum(ready_cities$Freq)/sum(cities)

categories <- dat$main_category
categories <- table(categories)
ready_categories <- data.frame(categories[order(categories, decreasing = FALSE)])

example <- unique(dat$it_location)[order(unique(dat$it_location))][61:80]

